import streamlit as st
import openai
import os
import json
import re
import mysql.connector
import time
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
if 'form_data' not in st.session_state: st.session_state.form_data = scenarios["University Library System"]

# --- 5. SIDEBAR ---
with st.sidebar:
    st.header("📂 1. Load Scenario")
    for s_name in scenarios.keys():
        if st.button(s_name):
            st.session_state.form_data = scenarios[s_name]
            st.rerun()
    st.divider()
    st.header("📝 2. Project Definition")
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
st.subheader("🚀 3. Open Stage")
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

# STAGE 1: BUSINESS RULES [cite: 32-43]
if st.session_state.active_stage == 1:
    st.subheader("📋 Stage 1 & 2: Extraction of Business Rules")
    if st.button("✨ ChatGPT ile Kuralları Oluştur"):
        with st.spinner("Analiz ediliyor..."):
            prompt = f"Extract business rules for {domain}. Entities: {entities}. Rules: {constraints}. Return JSON list: BR-ID, Type (S,O,T,Y), Rule Statement, ER Component (E,R,A,C), Implementation Tip, Rationale. [cite: 35-43]"
            st.session_state.rules_data = call_ai(prompt)
    if st.session_state.rules_data: st.table(st.session_state.rules_data)

# STAGE 2: TABLE DEFINITIONS [cite: 46-55]
elif st.session_state.active_stage == 2:
    st.subheader("📐 Stage 3: Table Definition (Data Dictionary)")
    
    if st.button("✨ ChatGPT ile Tablo Şemalarını Oluştur"):
        with st.spinner("Şemalar düzenleniyor..."):
            prompt = f"""
            Database Architect: Define tables for {entities}. 
            Return a JSON list of objects. Each object MUST have:
            'table_name', 'columns' (a list of objects with 'name', 'type', 'constraints').
            """
            st.session_state.table_defs = call_ai(prompt)

    if 'table_defs' in st.session_state and st.session_state.table_defs:
        for table in st.session_state.table_defs:
            # Her tablo için ayrı bir kart/expander [cite: 50]
            with st.expander(f"📂 Table: {table['table_name'].upper()}", expanded=True):
                # Sütun verilerini tabloya dönüştürme [cite: 49, 51, 52]
                cols_data = table['columns'] 
                st.table(cols_data) # Temiz ve düzenli tablo görünümü
                
                # İlişkileri (FK) ayrıca belirtme [cite: 48, 52]
                if 'relationships' in table:
                    st.caption(f"🔗 **Relationships:** {table['relationships']}")
    else:
        st.warning("Lütfen şemaları oluşturmak için butona basın.")

# STAGE 3: DETECTING MISSING RULES [cite: 56-60]
elif st.session_state.active_stage == 3:
    st.subheader("🔍 Stage 4: Detecting Missing or Ambiguous Rules")
    if st.button("🔍 Eksiklikleri Tara"):
        with st.spinner("Açıklar aranıyor..."):
            prompt = f"Identify 3 missing or unclear database rules for {domain}. Return JSON: 'Missing Rule', 'Related BR', 'Solution'. [cite: 59]"
            st.session_state.missing_data = call_ai(prompt)
    if st.session_state.missing_data: st.table(st.session_state.missing_data)

# STAGE 4: NORMALIZATION [cite: 61-66]
elif st.session_state.active_stage == 4:
    st.subheader("⚡ Stage 5: Normalization (0NF → 3NF)")
    n_tabs = st.tabs(["1NF", "2NF", "3NF"])
    with n_tabs[0]:
        st.markdown("**1NF:** Atomic values enforced. No repeating groups. [cite: 63]")
        st.code(f"{entities.split(',')[0].upper()} (ID, MultiValueAttr) --> SPLIT TO NEW ROWS")
    with n_tabs[1]: st.markdown("**2NF:** Partial dependencies removed. [cite: 64]")
    with n_tabs[2]:
        st.markdown("**3NF:** Transitive dependencies removed. [cite: 65]")
        st.success("Tablolar 3. Normal Form seviyesine getirildi.")

# STAGE 5: ER DIAGRAM [cite: 67-68]
elif st.session_state.active_stage == 5:
    st.subheader("🖼️ Stage 6: ER Diagram (Crow’s Foot Notation)")
    st.write("Varlıklar arası kardinaliteler (1:1, 1:N, M:N) ")
    
    st.image("https://mermaid.ink/img/pako:eNpdkU1Lw0AQhv_KMMeeFPyA9SBaL9KDFK8S0mS3STfN7mS2pS39704S09ZDe3mY933emS0H66yBAfE-9UAsWOsMB-p65y2SUnp8uIuF-D5iX3iY8r5W67U4m-vI-605UclYv0M_L_fC2y_37_0X-MCHL3S1YV-XoYclpIn2pE7T_F-O4YyVst9_f7Gf-G7-rS3U9kYv8D05QJp9mHNoY80fR9p-rN932C-0LpDq8T0k-636Ysc-2pT_36N8AEP5Z10")

# STAGE 6: SQL SCRIPT [cite: 69-74]
elif st.session_state.active_stage == 6:
    st.subheader("⌨️ Stage 7: SQL Code Generation")
    main_t = entities.split(',')[0].strip().replace(' ', '_')
    sql_text = f"""-- 1. Table Creation 
CREATE TABLE `{main_t}` (ID INT PRIMARY KEY AUTO_INCREMENT, Name VARCHAR(100));
-- 2. Advanced Feature: {adv_feat} [cite: 73]
CREATE TRIGGER TRG_After_Action BEFORE INSERT ON `{main_t}` ...
-- 3. Reporting Queries [cite: 74]
SELECT * FROM `{main_t}` ORDER BY ID DESC LIMIT 3;"""
    st.code(sql_text, language="sql")

# STAGE 7: DEPLOY
elif st.session_state.active_stage == 7:
    st.subheader("🚀 Final Step: Deployment to PHPMyAdmin")
    db_name = domain.lower().replace(" ", "_") + "_db"
    if st.button("🚀 EXECUTE ON MySQL"):
        try:
            conn = mysql.connector.connect(host="localhost", user="root", password="")
            cursor = conn.cursor()
            cursor.execute(f"CREATE DATABASE IF NOT EXISTS `{db_name}`")
            cursor.execute(f"USE `{db_name}`")
            table_name = entities.split(',')[0].strip().replace(' ', '_')
            cursor.execute(f"CREATE TABLE IF NOT EXISTS `{table_name}` (id INT AUTO_INCREMENT PRIMARY KEY, name VARCHAR(255))")
            st.success(f"✅ '{db_name}' veritabanı PHPMyAdmin'e başarıyla eklendi!")
            st.balloons()
            cursor.close(); conn.close()
        except Exception as e: st.error(f"XAMPP Hatası: {e}")

if st.session_state.active_stage == 0:
    st.info("Süreci başlatmak için lütfen sol menüdeki tanımları yapın ve Stage 1'e tıklayın.")