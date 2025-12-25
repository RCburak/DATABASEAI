import streamlit as st
import openai
import os
import json
import re
import mysql.connector
import time
import base64
from dotenv import load_dotenv

# --- 1. KONFİGÜRASYON ---
load_dotenv()
api_key = os.getenv("OPENAI_API_KEY")
client = openai.OpenAI(api_key=api_key)

st.set_page_config(page_title="Modular Database Architect AI", layout="wide")

# --- 2. CSS TASARIMI ---
st.markdown("""
    <style>
    .stApp { background-color: #0d1117; color: #c9d1d9; }
    [data-testid="stSidebar"] { background-color: #010409; border-right: 1px solid #30363d; }
    .metric-card { background: #161b22; border: 1px solid #30363d; border-radius: 8px; padding: 15px; text-align: center; }
    div.stButton > button { width: 100%; border-radius: 6px; height: 48px; font-weight: 600; background-color: #21262d; color: #c9d1d9; border: 1px solid #30363d; }
    div.stButton > button[kind="primary"] { background: #1f6feb !important; border: none !important; color: white !important; }
    </style>
    """, unsafe_allow_html=True)

# --- 3. SENARYOLAR ---
scenarios = {
    "University Library System": {
        "domain": "University Library System", "entities": "Books, Students, Loans, Authors",
        "constraints": "Max 3 books per student. 15 days loan.", "advanced": "Auto-calculate fine trigger",
        "security": "Librarian: Full Access, Student: Read", "reporting": "Overdue reports", "tasks": "Borrow, Return"
    },
    "E-Commerce Platform": {
        "domain": "E-Commerce Platform", "entities": "Products, Customers, Orders, Categories",
        "constraints": "Minimum order $10. Stock must be positive.", "advanced": "Auto-update inventory",
        "security": "Admin: Full, User: Self-orders only", "reporting": "Sales revenue", "tasks": "Add to Cart, Checkout"
    },
    "Hospital Management": {
        "domain": "Hospital Management", "entities": "Doctors, Patients, Appointments, Records",
        "constraints": "No overlapping doctor appointments.", "advanced": "Auto-generate invoice",
        "security": "Doctor: Medical records, Reception: Schedules", "reporting": "Daily patient count", "tasks": "Register, Book"
    }
}

# --- 4. SESSION STATE ---
if 'active_stage' not in st.session_state: st.session_state.active_stage = 0
if 'rules_data' not in st.session_state: st.session_state.rules_data = []
if 'table_defs' not in st.session_state: st.session_state.table_defs = []
if 'missing_data' not in st.session_state: st.session_state.missing_data = []
if 'full_sql' not in st.session_state: st.session_state.full_sql = ""
if 'form_data' not in st.session_state: st.session_state.form_data = scenarios["University Library System"]

# --- 5. SIDEBAR (VERİ YÖNETİM PANELİ VE TANIMLAR) ---
with st.sidebar:
    # BÖLÜM 1: VERİ YÖNETİMİ
    st.header("⚙️ 1. Veri Yönetim Paneli")
    
    # Mevcut tanımlardan veritabanı ismini al (Hata almamak için form_data kullanıldı)
    db_name = st.session_state.form_data["domain"].lower().replace(" ", "_") + "_db"
    
    # ELLE VERİ EKLEME (INSERT)
    st.markdown("### ➕ Manuel Kayıt Ekle")
    with st.form("sidebar_insert_form"):
        # Entities girişinden tabloları listele
        target_list = [e.strip() for e in st.session_state.form_data["entities"].split(',')]
        target_table = st.selectbox("Tablo Seç", target_list)
        new_entry_name = st.text_input("Kayıt Adı (Örn: Harry Potter)")
        submit_ins = st.form_submit_button("Veritabanına Kaydet")
        
        if submit_ins and new_entry_name:
            try:
                conn = mysql.connector.connect(host="localhost", user="root", password="", database=db_name)
                cursor = conn.cursor()
                formatted_table = target_table.replace(' ', '_')
                cursor.execute(f"INSERT INTO `{formatted_table}` (name) VALUES (%s)", (new_entry_name,))
                conn.commit()
                st.sidebar.success(f"✅ {new_entry_name} eklendi!")
                cursor.close(); conn.close()
            except Exception as e:
                st.sidebar.error("Önce 'Deploy' aşamasını tamamlayın!")

    # VERİTABANINI SİLME (DROP)
    if st.button("🚨 TÜM VERİTABANINI SİL"):
        try:
            conn = mysql.connector.connect(host="localhost", user="root", password="")
            cursor = conn.cursor()
            cursor.execute(f"DROP DATABASE IF EXISTS `{db_name}`")
            st.sidebar.warning(f"💥 `{db_name}` tamamen silindi.")
            st.session_state.active_stage = 0
            time.sleep(0.5); st.rerun()
        except Exception as e: st.sidebar.error(f"Hata: {e}")

    st.divider()

    # BÖLÜM 2: SENARYO YÜKLEME
    st.header("📂 2. Load Scenario")
    for s_name in scenarios.keys():
        if st.button(s_name):
            st.session_state.form_data = scenarios[s_name]
            st.rerun()

    st.divider()

    # BÖLÜM 3: PROJE TANIMI
    st.header("📝 3. Project Definition")
    domain = st.text_input("Domain", st.session_state.form_data["domain"])
    entities = st.text_input("Primary Entity focus", st.session_state.form_data["entities"])
    constraints = st.text_area("Constraint/Rule", st.session_state.form_data["constraints"])
    adv_feat = st.text_input("Advanced Feature / Trigger", st.session_state.form_data["advanced"])
    security = st.text_input("Security / Access Control", st.session_state.form_data["security"])
    reporting = st.text_input("Reporting Requirement", st.session_state.form_data["reporting"])
    tasks = st.text_input("Common Tasks", st.session_state.form_data["tasks"])

# --- 6. TOP DASHBOARD ---
st.title("🏗️ Modular Database Architect AI")
c1, c2, c3, c4 = st.columns(4)
with c1: st.markdown(f'<div class="metric-card"><small>DOMAIN</small><br><b>{domain}</b></div>', unsafe_allow_html=True)
with c2: st.markdown(f'<div class="metric-card"><small>ENTITIES</small><br><b>{entities}</b></div>', unsafe_allow_html=True)
with c3: st.markdown(f'<div class="metric-card"><small>STAGE</small><br><b>{st.session_state.active_stage}</b></div>', unsafe_allow_html=True)
with c4: st.markdown(f'<div class="metric-card"><small>AI STATUS</small><br><b>Ready</b></div>', unsafe_allow_html=True)

st.write("##")
st.subheader("🚀 4. Open Stage")
btn_cols = st.columns(7)
stage_names = ["Business Rules", "Table Defs", "Fix & Missing", "Normalization", "ER Diagram", "SQL Script", "Deploy"]

for i, name in enumerate(stage_names, 1):
    if btn_cols[i-1].button(f"{i}. {name}", type="primary" if st.session_state.active_stage == i else "secondary"):
        st.session_state.active_stage = i
        st.rerun()
st.divider()

# --- 7. AI HELPER ---
def call_ai(prompt):
    try:
        response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
        content = response.choices[0].message.content
        match = re.search(r'\[.*\]', content, re.DOTALL)
        return json.loads(match.group(0)) if match else None
    except: return None

# --- 8. STAGE CONTENT ---

# STAGE 1: BUSINESS RULES
if st.session_state.active_stage == 1:
    st.subheader("📋 Stage 1 & 2: Extraction of Business Rules")
    if st.button("✨ ChatGPT ile Kuralları Oluştur"):
        with st.spinner("Analiz ediliyor..."):
            prompt = f"Extract business rules for {domain}. Entities: {entities}. Rules: {constraints}. Return JSON list: BR-ID, Type (S,O,T,Y), Rule Statement, ER Component (E,R,A,C), Implementation Tip, Rationale."
            st.session_state.rules_data = call_ai(prompt)
    if st.session_state.rules_data: st.table(st.session_state.rules_data)

# STAGE 2: TABLE DEFINITIONS
elif st.session_state.active_stage == 2:
    st.subheader("📐 Stage 3: Table Definition (Data Dictionary)")
    if st.button("✨ ChatGPT ile Tablo Şemalarını Oluştur"):
        with st.spinner("Şemalar düzenleniyor..."):
            prompt = f"Database Architect: Define pluralized tables for {entities}. Return JSON list of objects: 'table_name', 'columns' (list of objects with 'name', 'type', 'constraints')."
            st.session_state.table_defs = call_ai(prompt)
    if st.session_state.table_defs:
        for table in st.session_state.table_defs:
            with st.expander(f"📂 Table: {table['table_name'].upper()}", expanded=True):
                st.table(table['columns'])

# STAGE 5: ER DIAGRAM (Base64 Fix)
elif st.session_state.active_stage == 5:
    st.subheader("🖼️ Stage 6: ER Diagram (Crow’s Foot Notation)")
    if st.button("✨ ER Diyagramını Oluştur"):
        with st.spinner("Şema analiz ediliyor..."):
            prompt = f"Generate Mermaid.js ER diagram for {domain} with entities {entities}. Crow's Foot notation. Return ONLY code block."
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            mermaid_code = response.choices[0].message.content.replace("```mermaid", "").replace("```", "").strip()
            encoded_string = base64.b64encode(mermaid_code.encode('utf-8')).decode('utf-8')
            st.image(f"https://mermaid.ink/img/{encoded_string}", use_container_width=True)

# STAGE 6: SQL SCRIPT
elif st.session_state.active_stage == 6:
    st.subheader("⌨️ Stage 7: SQL Code Generation")
    if st.button("✨ ChatGPT ile Tam SQL Betiği Üret"):
        with st.spinner("Kodlanıyor..."):
            prompt = f"Senior DBA: Generate full MySQL script for {domain}. Entities: {entities}. Trigger for {adv_feat}. Use backticks. Semicolon separated."
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            st.session_state.full_sql = response.choices[0].message.content.replace("```sql", "").replace("```", "").strip()
    if st.session_state.full_sql: st.code(st.session_state.full_sql, language="sql")

# STAGE 7: DEPLOY
elif st.session_state.active_stage == 7:
    st.subheader("🚀 Final Step: Deployment to PHPMyAdmin")
    if st.button("🚀 EXECUTE ON MySQL"):
        try:
            conn = mysql.connector.connect(host="localhost", user="root", password="")
            cursor = conn.cursor()
            cursor.execute(f"CREATE DATABASE IF NOT EXISTS `{db_name}`")
            cursor.execute(f"USE `{db_name}`")
            # Çoklu sorguları güvenli çalıştırma
            for q in [x.strip() for x in st.session_state.full_sql.split(';') if x.strip()]:
                cursor.execute(q)
            st.success(f"✅ `{db_name}` PHPMyAdmin'e aktarıldı!")
            st.balloons(); cursor.close(); conn.close()
        except Exception as e: st.error(f"XAMPP Hatası: {e}")

if st.session_state.active_stage == 0:
    st.info("Süreci başlatmak için lütfen sol menüdeki tanımları yapın ve Stage 1'e tıklayın.")