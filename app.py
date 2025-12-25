import streamlit as st
import openai
import os
import json
import re
import mysql.connector
import time
from dotenv import load_dotenv
from streamlit_mermaid import st_mermaid

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
    # --- 1. DATABASE CONTROL ---
    st.header("⚙️ 1. Database Control")
    
    temp_db_name = st.session_state.form_data["domain"].lower().replace(" ", "_") + "_db"
    
    # VERİTABANINI SIFIRLAMA (DROP DATABASE)
    if st.button("🚨 TÜM VERİTABANINI SİL"):
        try:
            conn = mysql.connector.connect(host="localhost", user="root", password="")
            cursor = conn.cursor()
            cursor.execute(f"DROP DATABASE IF EXISTS `{temp_db_name}`")
            st.sidebar.warning(f"💥 `{temp_db_name}` veritabanı silindi.")
            st.session_state.active_stage = 0
            time.sleep(0.5)
            st.rerun()
        except Exception as e:
            st.sidebar.error(f"Bağlantı Hatası: {e}")

    st.divider()

    # --- 2. LOAD SCENARIO ---
    st.header("📂 2. Load Scenario")
    for s_name in scenarios.keys():
        if st.button(s_name, key=f"side_{s_name}"):
            st.session_state.form_data = scenarios[s_name]
            st.rerun()
            
    st.divider()

    # --- 3. PROJECT DEFINITION ---
    st.header("📝 3. Project Definition")
    domain = st.text_input("Domain", st.session_state.form_data["domain"])
    entities = st.text_input("Primary Entity focus", st.session_state.form_data["entities"])
    constraints = st.text_area("Constraint/Rule", st.session_state.form_data["constraints"])
    adv_feat = st.text_input("Advanced Feature / Trigger", st.session_state.form_data["advanced"])
    security = st.text_input("Security / Access Control", st.session_state.form_data["security"])
    reporting = st.text_input("Reporting Requirement", st.session_state.form_data["reporting"])
    tasks = st.text_input("Common Tasks", st.session_state.form_data["tasks"])
    
    st.session_state.form_data["domain"] = domain
    st.session_state.form_data["entities"] = entities
    st.session_state.form_data["constraints"] = constraints

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
            prompt = f"""
            Database Architect: Define tables for {entities}. 
            Return a JSON list of objects. Each object MUST have:
            'table_name', 'columns' (a list of objects with 'name', 'type', 'constraints').
            """
            st.session_state.table_defs = call_ai(prompt)

    if 'table_defs' in st.session_state and st.session_state.table_defs:
        for table in st.session_state.table_defs:
            with st.expander(f"📂 Table: {table['table_name'].upper()}", expanded=True):
                cols_data = table['columns'] 
                st.table(cols_data)
                if 'relationships' in table:
                    st.caption(f"🔗 **Relationships:** {table['relationships']}")
    else:
        st.warning("Lütfen şemaları oluşturmak için butona basın.")

# STAGE 3: DETECTING & FIXING MISSING RULES
elif st.session_state.active_stage == 3:
    st.subheader("🔍 Stage 4: Detecting & Fixing Missing Rules")
    col1, col2 = st.columns(2)
    with col1:
        if st.button("🔍 1. Adım: Eksiklikleri Tara"):
            with st.spinner("Mantıksal açıklar ve eksik kurallar aranıyor..."):
                prompt = f"""
                Analyze the database rules for {domain}. 
                Current Entities: {entities}. 
                Current Constraints: {constraints}.
                Identify 3 missing or unclear database rules that are critical for integrity.
                Return JSON list with keys: 'Missing Rule', 'Related BR', 'Solution'.
                """
                st.session_state.missing_data = call_ai(prompt)
        if st.session_state.missing_data:
            st.markdown("### 📋 Tespit Edilen Eksiklikler")
            st.table(st.session_state.missing_data)

    with col2:
        if st.session_state.missing_data:
            if st.button("🔧 2. Adım: Kuralları Fixle ve Business Rules'a Ekle"):
                with st.spinner("Eksik kurallar ana listeye entegre ediliyor..."):
                    current_rules_text = str(st.session_state.rules_data)
                    missing_rules_text = str(st.session_state.missing_data)
                    prompt = f"""
                    Merge these two lists into one comprehensive business rules list for {domain}.
                    List 1 (Current): {current_rules_text}
                    List 2 (Missing to Add): {missing_rules_text}
                    Return a SINGLE JSON list: BR-ID, Type (S,O,T,Y), Rule Statement, ER Component (E,R,A,C), Implementation Tip, Rationale.
                    """
                    updated_rules = call_ai(prompt)
                    if updated_rules:
                        st.session_state.rules_data = updated_rules
                        st.success("✅ Eksik kurallar başarıyla 'Business Rules' aşamasına eklendi!")
                        st.balloons()
        else:
            st.info("Önce eksiklikleri taramanız gerekmektedir.")

    if st.session_state.rules_data:
        with st.expander("Güncel Business Rules Listesini Gör"):
            st.table(st.session_state.rules_data)

# STAGE 4: NORMALIZATION
elif st.session_state.active_stage == 4:
    st.subheader("⚡ Stage 5: Normalization (0NF → 3NF)")
    st.info("Veritabanı tasarımı, veri tekrarını önlemek için 3. Normal Form seviyesine getiriliyor.")
    n_tabs = st.tabs(["1NF (Atomic)", "2NF (Partial Dep)", "3NF (Transitive Dep)"])
    
    with n_tabs[0]:
        st.markdown("#### 1NF: First Normal Form")
        main_entity = entities.split(',')[0].strip().upper()
        st.code(f"-- 1NF Örneği:\n{main_entity} (ID, Name)\n{main_entity}_PHONES (ID, PhoneNumber)", language="sql")
        st.success("✅ Veriler atomik hale getirildi.")

    with n_tabs[1]:
        st.markdown("#### 2NF: Second Normal Form")
        st.code("-- 2NF Örneği:\nORDERS (OrderID, OrderDate)\nORDER_ITEMS (OrderID, ProductID, Price)", language="sql")
        st.success("✅ Kısmi fonksiyonel bağımlılıklar giderildi.")

    with n_tabs[2]:
        st.markdown("#### 3NF: Third Normal Form")
        st.code("-- 3NF Örneği:\nSTUDENTS (StudentID, Name, DeptID)\nDEPARTMENTS (DeptID, DeptName)", language="sql")
        st.success("✅ Geçişli bağımlılıklar kaldırıldı.")

# STAGE 5: ER DIAGRAM
elif st.session_state.active_stage == 5:
    st.subheader("🖼️ Stage 6: ER Diagram (Crow’s Foot Notation)")
    if st.button("✨ ER Diyagramını Oluştur"):
        with st.spinner("Şema analiz ediliyor..."):
            prompt = f"Generate a Mermaid.js ER diagram using Crow's Foot notation for Domain: {domain}, Entities: {entities}. Return ONLY mermaid code."
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            mermaid_code = response.choices[0].message.content.replace("```mermaid", "").replace("```", "").strip()
            import base64
            encoded_string = base64.b64encode(mermaid_code.encode('utf-8')).decode('utf-8')
            st.image(f"https://mermaid.ink/img/{encoded_string}", use_container_width=True)
            st.success("ER Diyagramı başarıyla oluşturuldu!")

# STAGE 6: SQL SCRIPT
elif st.session_state.active_stage == 6:
    st.subheader("⌨️ Stage 7: SQL Code Generation")
    if st.button("✨ ChatGPT ile Tam SQL Betiği Üret"):
        with st.spinner("Veritabanı mimarisi SQL'e dönüştürülüyor..."):
            prompt = f"As a Senior DBA, generate a full MySQL script for {domain}. Entities: {entities}. Requirement: {reporting}."
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            st.session_state.full_sql = response.choices[0].message.content.replace("```sql", "").replace("```", "").strip()

    if 'full_sql' in st.session_state:
        st.code(st.session_state.full_sql, language="sql")
        st.download_button("📄 SQL Dosyasını İndir", st.session_state.full_sql, file_name="schema.sql")

# STAGE 7: DEPLOY
elif st.session_state.active_stage == 7:
    st.subheader("🚀 Final Step: Deployment to PHPMyAdmin")
    db_name = domain.lower().replace(" ", "_") + "_db"
    if st.button("🚀 EXECUTE ON MySQL"):
        try:
            conn = mysql.connector.connect(host="localhost", user="root", password="")
            cursor = conn.cursor()
            cursor.execute(f"CREATE DATABASE IF NOT EXISTS `{db_name}`")
            st.success(f"✅ '{db_name}' veritabanı PHPMyAdmin'e başarıyla eklendi!")
            st.balloons()
            cursor.close(); conn.close()
        except Exception as e: st.error(f"Bağlantı Hatası: {e}")

if st.session_state.active_stage == 0:
    st.info("Süreci başlatmak için lütfen sol menüdeki tanımları yapın ve Stage 1'e tıklayın.")