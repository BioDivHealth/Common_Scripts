# -*- coding: utf-8 -*-
"""BirdPDF_Zephyr_and_Gradio

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1Q4wV1959hiZnjs0GNoG_isQaGz-bL_Vm
"""

#install required packages
!pip install -q transformers peft  accelerate safetensors sentencepiece streamlit chromadb langchain sentence-transformers gradio pypdf
!pip install -i https://pypi.org/simple/ bitsandbytes

# fixing unicode error in google colab (https://levelup.gitconnected.com/building-a-private-ai-chatbot-2c071f6715ad)
import locale
locale.getpreferredencoding = lambda: "UTF-8"

from google.colab import userdata
userdata.get('huggingface')

HF_TOKEN = "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"

# import dependencies for text import, llm and gradio app
import torch
from transformers import AutoModelForCausalLM, AutoTokenizer, BitsAndBytesConfig, pipeline

import os
import gradio as gr
from google.colab import drive

import chromadb
from langchain.llms import HuggingFacePipeline
from langchain.document_loaders import TextLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.embeddings import HuggingFaceEmbeddings
from langchain.vectorstores import Chroma
from langchain import HuggingFacePipeline
from langchain.document_loaders import PyPDFDirectoryLoader
from langchain.chains import ConversationalRetrievalChain
from langchain.memory import ConversationBufferMemory

"""Next, download the Zephyr-7B-Alpha model, which consumes around 5GB of RAM on the free Google Colab notebook. This process typically takes between 5 to 10 minutes when utilizing the T4 GPU. **Will not work if you just run on  CPU!**


"""

# specify model huggingface mode name
model_name =  "anakin87/zephyr-7b-alpha-sharded"

###### other models:
# "Trelis/Llama-2-7b-chat-hf-sharded-bf16"
# "bn22/Mistral-7B-Instruct-v0.1-sharded"
# "HuggingFaceH4/zephyr-7b-beta"

# function for loading 4-bit quantized model
def load_quantized_model(model_name: str):
    """
    :param model_name: Name or path of the model to be loaded.
    :return: Loaded quantized model.
    """
    bnb_config = BitsAndBytesConfig(
        load_in_4bit=True,
        bnb_4bit_use_double_quant=True,
        bnb_4bit_quant_type="nf4",
        bnb_4bit_compute_dtype=torch.bfloat16
    )

    model = AutoModelForCausalLM.from_pretrained(
        model_name,
        load_in_4bit=True,
        torch_dtype=torch.bfloat16,
        quantization_config=bnb_config
    )
    return model

"""Prepare function for tokenising the input text"""

# fucntion for initializing tokenizer
def initialize_tokenizer(model_name: str):
    """
    Initialize the tokenizer with the specified model_name.

    :param model_name: Name or path of the model for tokenizer initialization.
    :return: Initialized tokenizer.
    """
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    tokenizer.bos_token_id = 1  # Set beginning of sentence token id
    return tokenizer

"""Load and run model. Again, this **will not run on CPU. Works best on T4 GPU.**"""

from google.colab import userdata
userdata.get('huggingface')

# load model
model = load_quantized_model(model_name)

# initialize tokenizer
tokenizer = initialize_tokenizer(model_name)

# specify stop token ids
stop_token_ids = [0]

# Save the tokenizer
tokenizer.save_pretrained("/content/drive/MyDrive/ebirdAI/Scripts/models'")

# Load tokenizer from saved directory
tokenizer = BertTokenizer.from_pretrained("path_to_saved_tokenizer")

"""Connect to  google drive folder of choice (here I'm just using some pdfs about bird communities in Northern Ireland but theoretically this will read a whole book)."""

# mount google drive and specify folder path
drive.mount('/content/drive')
folder_path = '/content/drive/MyDrive/bird_pdfs' # add the appropriate file path here

"""Now that the documents are accessible in colab,  load and segment them into smaller text chunks. Embed these chunks into the vector database Chroma DB using HuggingFace Embeddings and Langchain tools.


"""



# load pdf files
loader = PyPDFDirectoryLoader(folder_path)
documents = loader.load()

# split the documents in small chunks
text_splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=100) #Change the chunk_size and chunk_overlap as needed
all_splits = text_splitter.split_documents(documents)

# specify embedding model (using huggingface sentence transformer)
embedding_model_name = "sentence-transformers/all-mpnet-base-v2"
model_kwargs = {"device": "cuda"}
embeddings = HuggingFaceEmbeddings(model_name=embedding_model_name, model_kwargs=model_kwargs)

#embed document chunks
vectordb = Chroma.from_documents(documents=all_splits, embedding=embeddings, persist_directory="chroma_db")

# specify the retriever
retriever = vectordb.as_retriever()

"""With the documents embedded in the vector database, next we construct a RAG (retrieval augmented generation) pipeline using HuggingFace and Langchain.

"""

# build huggingface pipeline for using zephyr-7b-alpha
pipeline = pipeline(
        "text-generation",
        model=model,
        tokenizer=tokenizer,
        use_cache=True,
        device_map="auto",
        max_length=2048,
        do_sample=True,
        top_k=5,
        num_return_sequences=1,
        eos_token_id=tokenizer.eos_token_id,
        pad_token_id=tokenizer.eos_token_id,
)

# specify the llm
llm = HuggingFacePipeline(pipeline=pipeline)

# build conversational retrieval chain with memory (rag) using langchain
def create_conversation(query: str, chat_history: list) -> tuple:
    try:

        memory = ConversationBufferMemory(
            memory_key='chat_history',
            return_messages=False
        )
        qa_chain = ConversationalRetrievalChain.from_llm(
            llm=llm,
            retriever=retriever,
            memory=memory,
            get_chat_history=lambda h: h,
        )

        result = qa_chain({'question': query, 'chat_history': chat_history})
        chat_history.append((query, result['answer']))
        return '', chat_history


    except Exception as e:
        chat_history.append((query, e))
        return '', chat_history

"""Now we use the Gradio package to generate a new chatbot!"""

# build gradio ui
with gr.Blocks() as demo:

    chatbot = gr.Chatbot(label='Chat with the BirdGPT)')
    msg = gr.Textbox()
    clear = gr.ClearButton([msg, chatbot])

    msg.submit(create_conversation, [msg, chatbot], [msg, chatbot])

demo.launch()