import os
from fastapi import FastAPI
from pydantic import BaseModel
from langchain_core.output_parsers import StrOutputParser
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.document_loaders import TextLoader
from langchain_openai import OpenAIEmbeddings, ChatOpenAI
from langchain_chroma import Chroma
from langchain.prompts import PromptTemplate
from dotenv import load_dotenv

load_dotenv()

# Set environment variables
os.environ["LANGCHAIN_TRACING_V2"] = "true"
os.environ["LANGCHAIN_API_KEY"] = os.getenv("LANGCHAIN_API_KEY")
os.environ["OPENAI_API_KEY"] = os.getenv("OPENAI_API_KEY")

# Initialize FastAPI app
app = FastAPI()

# -----------------------------
# Load and Split Knowledge Base
# -----------------------------

ruleset_loader = TextLoader("ruleset.txt")
documents = ruleset_loader.load()
text_splitter = RecursiveCharacterTextSplitter(chunk_size=500, chunk_overlap=0)
docs = text_splitter.split_documents(documents)

abap_exmpl_loader = TextLoader("abap_program.txt")
exmpl_abap = abap_exmpl_loader.load()
text_splitter2 = RecursiveCharacterTextSplitter(chunk_size=500, chunk_overlap=0)
docs2 = text_splitter2.split_documents(exmpl_abap)

# Merge rule and example documents
all_docs = docs + docs2

# -----------------------------
# Embeddings + Vector Store
# -----------------------------

persist_directory = "./chroma_db"
embeddings = OpenAIEmbeddings()

vectorstore = Chroma.from_documents(
    documents=all_docs,
    embedding=embeddings,
    persist_directory=persist_directory
)

retriever = vectorstore.as_retriever(
    search_type="similarity_score_threshold",
    search_kwargs={"score_threshold": 0.2}
)

# -----------------------------
# LangChain Prompt + LLM Setup
# -----------------------------

llm = ChatOpenAI(
    model="gpt-4.1",         # ✅ GPT-4o supports 128K context
    temperature=0,
)

# Step 1 - Identify Rules
identify_prompt = PromptTemplate(
    input_variables=["rules", "input_code"],
    template="""
You are an SAP ABAP Remediation Assistant.

Context:
{rules}

Task:
- Analyze the ECC ABAP code.
- List applicable rules (Rule No and Title).
- Do not provide Remediated ABAP Code.

ECC ABAP Code:
{input_code}

Output:
Applicable Rules: [Rule 1: Title, Rule 2: Title, etc.]
"""
)
identify_parser = StrOutputParser()
identify_chain = identify_prompt | llm | identify_parser


# Step 2 - Remediate Code
remediate_prompt = PromptTemplate(
    input_variables=["Rules", "applicable_rules", "example_rules", "input_code"],
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

Rules:
{Rules}

Applicable Rules:
{applicable_rules}

Example Rules:
{example_rules}

ECC ABAP Code:
{input_code}

Output:
[Remediated ABAP Code]
"""
)
remediate_parser = StrOutputParser()
remediate_chain = remediate_prompt | llm | remediate_parser

# -----------------------------
# Pydantic Input Model
# -----------------------------

class ABAPCodeInput(BaseModel):
    code: str

# -----------------------------
# Core Remediation Function
# -----------------------------

def remediate_abap_with_validation(input_code: str):
    # Get rules and examples as strings
    rules_text = "\n\n".join([doc.page_content for doc in docs])
    example_rules_text = "\n\n".join([doc.page_content for doc in docs2])

    # Step 1: Identify applicable rules
    applicable_rules = identify_chain.invoke({
        "rules": rules_text,
        "input_code": input_code
    })

    lines = input_code.splitlines()
    chunks = [lines[i:i+500] for i in range(0, len(lines), 500)]

    # Initialize result
    full_output = ""

    # Loop through each chunk and invoke LLM
    for chunk_lines in chunks:
        chunk_code = "\n".join(chunk_lines)
        remediated_chunk = remediate_chain.invoke({
            "Rules": rules_text,
            "applicable_rules": applicable_rules,
            "example_rules": example_rules_text,
            "input_code": chunk_code
        })
        full_output += remediated_chunk  # ✅ Only append raw remediated code

    # Save the final full output to file
    with open("remediated_output.abap", "w", encoding="utf-8") as f:
        f.write(full_output)

    return {"remediated_code": full_output}

# -----------------------------
# FastAPI Endpoint
# -----------------------------

@app.post("/remediate_abap/")
async def remediate_abap(input_data: ABAPCodeInput):
    result = remediate_abap_with_validation(input_data.code)
    return result
