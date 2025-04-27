import os
from fastapi import FastAPI
from pydantic import BaseModel
from langchain_core.output_parsers import StrOutputParser
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.document_loaders import TextLoader
from langchain_openai import OpenAIEmbeddings
from langchain_chroma import Chroma
from langchain.prompts import PromptTemplate
from langchain_openai import ChatOpenAI
from dotenv import load_dotenv
load_dotenv()

# SECRET_KEY = os.getenv("ANTHROPIC_API_KEY")
os.environ["LANGCHAIN_TRACING_V2"]="true"
os.environ["LANGCHAIN_API_KEY"]=os.getenv("LANGCHAIN_API_KEY")
os.environ["OPENAI_API_KEY"]=os.getenv("OPENAI_API_KEY")

# Initialize FastAPI app
app = FastAPI()

# -----------------------------
# RAG Setup: Load Ruleset
# -----------------------------

ruleset_loader = TextLoader("ruleset.txt")
documents = ruleset_loader.load()

# Split Rules into Chunks
text_splitter = RecursiveCharacterTextSplitter(
separators=["\n\n", "Rule"],
chunk_size=500,
chunk_overlap=0
)
docs = text_splitter.split_documents(documents)

# Embeddings + ChromaDB
persist_directory = "./chroma_db"
embeddings = OpenAIEmbeddings()

vectorstore = Chroma.from_documents(
documents=docs,
embedding=embeddings,
persist_directory=persist_directory
)

retriever = vectorstore.as_retriever(
search_type="similarity_score_threshold",
search_kwargs={"score_threshold": 0.2}
)

# -----------------------------
# LangChain Chains Setup
# -----------------------------

llm = ChatOpenAI(model="gpt-4o", temperature=0)


# Step 2 - Remediate Code
remediate_prompt = PromptTemplate(
input_variables=["applicable_rules", "input_code"],
template="""
You are an SAP ABAP Remediation Expert.

Task:
- Apply the following rules on the code.
- Comment out old code, insert new code.

Applicable Rules:
{applicable_rules}

ECC ABAP Code:
{input_code}

Output:
---
[Remediated ABAP Code]
---
"""
)
parser = StrOutputParser()
remediate_chain = remediate_prompt | llm | parser


# -----------------------------
# Pydantic Model
# -----------------------------

class ABAPCodeInput(BaseModel):
    code: str

# -----------------------------
# RAG Function
# -----------------------------

def remediate_abap_with_validation(input_code: str):
    # Retrieve Rules
    rules_text = "\n\n".join([doc.page_content for doc in docs])
    applicable_rules = rules_text  # Define applicable_rules from rules_text

    # Remediate Code
    remediated_code = remediate_chain.invoke({
        "applicable_rules": applicable_rules,
        "input_code": input_code
    })

    return {"remediated_code": remediated_code}


# -----------------------------
# FastAPI Endpoint
# -----------------------------

@app.post("/remediate_abap/")
async def remediate_abap(input_data: ABAPCodeInput):
    result = remediate_abap_with_validation(input_data.code)
    return result