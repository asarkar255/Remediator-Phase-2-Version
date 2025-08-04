import os
import re
from fastapi import FastAPI
from pydantic import BaseModel
from langchain_core.output_parsers import StrOutputParser
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.document_loaders import TextLoader
from langchain_openai import OpenAIEmbeddings, ChatOpenAI
from langchain_chroma import Chroma
from langchain.prompts import PromptTemplate
from langchain_core.runnables.history import RunnableWithMessageHistory
from langchain_core.chat_history import BaseChatMessageHistory
from langchain_core.chat_history import InMemoryChatMessageHistory
from dotenv import load_dotenv

load_dotenv()

# Set environment variables
os.environ["LANGCHAIN_TRACING_V2"] = "true"
os.environ["LANGCHAIN_API_KEY"] = os.getenv("LANGCHAIN_API_KEY")
os.environ["OPENAI_API_KEY"] = os.getenv("OPENAI_API_KEY")

app = FastAPI()

# -----------------------------
# Load Knowledge Base
# -----------------------------
ruleset_loader = TextLoader("ruleset.txt")
documents = ruleset_loader.load()
text_splitter = RecursiveCharacterTextSplitter(chunk_size=500, chunk_overlap=0)
docs = text_splitter.split_documents(documents)

abap_exmpl_loader = TextLoader("abap_program.txt")
exmpl_abap = abap_exmpl_loader.load()
text_splitter2 = RecursiveCharacterTextSplitter(chunk_size=500, chunk_overlap=0)
docs2 = text_splitter2.split_documents(exmpl_abap)

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
# LLM and Prompts
# -----------------------------
llm = ChatOpenAI(model="gpt-4.1", temperature=0)

# identify_prompt = PromptTemplate(
#     input_variables=["rules", "input_code"],
#     template="""
# You are an SAP ABAP Remediation Assistant.

# Context:
# {rules}

# Task:
# - Analyze the ECC ABAP code.
# - List applicable rules (Rule No and Title).
# - Do not provide Remediated ABAP Code.

# ECC ABAP Code:
# {input_code}

# Output:
# Applicable Rules: [Rule 1: Title, Rule 2: Title, etc.]
# """
# )

# identify_chain = identify_prompt | llm | StrOutputParser()

remediate_prompt = PromptTemplate(
    # input_variables=["Rules", "applicable_rules", "example_rules", "input_code"],
    input_variables=["Rules",  "example_rules", "input_code"],
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



Example Rules:
{example_rules}

ECC ABAP Code:
{input_code}

Output:
[Remediated ABAP Code]
"""
)

# -----------------------------
# Memory Management (Updated)
# -----------------------------
chat_histories = {}

def memory_factory(session_id: str) -> BaseChatMessageHistory:
    if session_id not in chat_histories:
        chat_histories[session_id] = InMemoryChatMessageHistory()
    return chat_histories[session_id]

remediate_chain = RunnableWithMessageHistory(
    remediate_prompt | llm | StrOutputParser(),
    memory_factory,
    input_messages_key="input_code",
    history_messages_key="history"
)

import re

def extract_global_declarations(remediated_code: str) -> str:
    pattern = re.compile(
        r"^\s*(DATA|TYPES|CONSTANTS|TABLES|PARAMETERS|SELECT-OPTIONS).*?\.\s*$",
        re.IGNORECASE | re.MULTILINE
    )
    matches = pattern.findall(remediated_code)
    lines = pattern.findall(remediated_code)
    return "\n".join(lines)

# FastAPI application for ABAP code remediation

# -----------------------------
# Pydantic Input
# -----------------------------
class ABAPCodeInput(BaseModel):
    code: str

# -----------------------------
# Core Function
# -----------------------------
def remediate_abap_with_validation(input_code: str):
    rules_text = "\n\n".join([doc.page_content for doc in docs])
    example_rules_text = "\n\n".join([doc.page_content for doc in docs2])

    lines = input_code.splitlines()
    chunks = [lines[i:i + 800] for i in range(0, len(lines), 800)]

    full_output = ""
    global_context = ""

    for idx, chunk_lines in enumerate(chunks):
        chunk_code = "\n".join(chunk_lines)

        if idx > 0 and global_context:
            chunk_code = global_context + "\n\n" + chunk_code

        response = remediate_chain.invoke(
            {
                "Rules": rules_text,
                "example_rules": example_rules_text,
                "input_code": chunk_code
            },
            config={"configurable": {"session_id": "default"}}
        )

        full_output += response

        # Extract context after first chunk
        if idx == 0:
            global_context = extract_global_declarations(response)

    return {"remediated_code": full_output}

# -----------------------------
# FastAPI Endpoint
# -----------------------------
@app.post("/remediate_abap/")
async def remediate_abap(input_data: ABAPCodeInput):
    return remediate_abap_with_validation(input_data.code)

