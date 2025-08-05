import os
from fastapi import FastAPI
from pydantic import BaseModel
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.document_loaders import TextLoader
from langchain_openai import OpenAIEmbeddings, ChatOpenAI
from langchain_chroma import Chroma
from langchain.prompts import PromptTemplate
from dotenv import load_dotenv

# Load env
load_dotenv()
os.environ["LANGCHAIN_API_KEY"] = os.getenv("LANGCHAIN_API_KEY")
os.environ["OPENAI_API_KEY"] = os.getenv("OPENAI_API_KEY")

app = FastAPI()

# -----------------------------
# Memory-cache global variables
# -----------------------------
rules_text = ""
example_rules_text = ""

# -----------------------------
# Load ruleset & example ABAP program ONCE at app startup
# -----------------------------
@app.on_event("startup")
def load_ruleset_in_memory():
    global rules_text, example_rules_text

    # Load rules
    ruleset_loader = TextLoader("ruleset.txt")
    rules_docs = ruleset_loader.load()
    rules_text = "\n\n".join([doc.page_content for doc in rules_docs])

    # Load examples
    example_loader = TextLoader("abap_program.txt")
    example_docs = example_loader.load()
    example_rules_text = "\n\n".join([doc.page_content for doc in example_docs])

# -----------------------------
# Define the model
# -----------------------------
llm = ChatOpenAI(model="gpt-4.1", temperature=0)

remediate_prompt = PromptTemplate(
    input_variables=["Rules", "global_variables", "example_rules", "input_code"],
    template="""
You are an SAP ABAP Remediation Expert.
Your task is to fully remediate all forms and subroutines in the ECC ABAP code.
DO NOT skip any section or write placeholders like "...rest is similar".
Comment out old code and insert new code following clean S/4HANA standards.

Apply the following:
- Comment legacy TABLES, OCCURS, LIKE, etc.
- Replace with DATA, TYPES, and modern SELECT.
- Follow all remediation rules strictly.
- Follow syntax and formatting exactly like examples.
- Ensure final output is complete and not trimmed.
- Always use global variables as defined in the input to follow the variables in the code.

Rules:
{Rules}

global_variables:
{global_variables}

Example Rules:
{example_rules}

ECC ABAP Code:
{input_code}

Output:
[Remediated ABAP Code]
"""
)

# -----------------------------
# Request Schema
# -----------------------------
class ABAPCodeInput(BaseModel):
    code: str
    global_variables: str

# -----------------------------
# Main Remediation Logic
# -----------------------------
def remediate_abap_with_validation(input_code: str, global_variables: str):
    lines = input_code.splitlines()
    chunk_size = 600
    chunks = [lines[i:i + chunk_size] for i in range(0, len(lines), chunk_size)]

    full_output = ""

    for idx, chunk_lines in enumerate(chunks):
        chunk_code = "\n".join(chunk_lines)

        prompt = remediate_prompt.format(
            Rules=rules_text,
            global_variables=global_variables,
            example_rules=example_rules_text,
            input_code=chunk_code
        )

        response = llm.invoke(prompt)
        full_output += response.content if hasattr(response, "content") else str(response)

    return {"remediated_code": full_output}

# -----------------------------
# FastAPI Endpoint
# -----------------------------
@app.post("/remediate_abap/")
async def remediate_abap(input_data: ABAPCodeInput):
    return remediate_abap_with_validation(input_data.code, input_data.global_variables)
