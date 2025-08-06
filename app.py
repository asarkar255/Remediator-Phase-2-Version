import os
from fastapi import FastAPI, Request
from contextlib import asynccontextmanager
from pydantic import BaseModel
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.document_loaders import TextLoader
from langchain_openai import OpenAIEmbeddings, ChatOpenAI
from langchain_chroma import Chroma
from langchain.prompts import PromptTemplate
from dotenv import load_dotenv

load_dotenv()
os.environ["LANGCHAIN_TRACING_V2"]="true"
os.environ["LANGCHAIN_API_KEY"] = os.getenv("LANGCHAIN_API_KEY")
os.environ["OPENAI_API_KEY"] = os.getenv("OPENAI_API_KEY")

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
# Use lifespan instead of @on_event("startup")
# -----------------------------
@asynccontextmanager
async def lifespan(app: FastAPI):
    print("🚀 Loading ruleset and examples into memory...")

    # Load rules once
    ruleset_loader = TextLoader("ruleset.txt")
    rules_docs = ruleset_loader.load()
    app.state.rules_text = "\n\n".join([doc.page_content for doc in rules_docs])

    # Load examples once
    example_loader = TextLoader("abap_program.txt")
    example_docs = example_loader.load()
    app.state.example_rules_text = "\n\n".join([doc.page_content for doc in example_docs])

    print("✅ Rules and examples loaded.")
    yield
    print("🛑 Shutting down app.")

# -----------------------------
# Initialize FastAPI with lifespan
# -----------------------------
app = FastAPI(lifespan=lifespan)

# -----------------------------
# Request Schema
# -----------------------------
class ABAPCodeInput(BaseModel):
    code: str
    global_variables: str

# -----------------------------
# Main Remediation Logic
# -----------------------------
def remediate_abap_with_validation(input_code: str, global_variables: str, rules_text: str, example_rules_text: str):
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
async def remediate_abap(input_data: ABAPCodeInput, request: Request):
    rules_text = request.app.state.rules_text
    example_rules_text = request.app.state.example_rules_text
    return remediate_abap_with_validation(input_data.code, input_data.global_variables, rules_text, example_rules_text)
