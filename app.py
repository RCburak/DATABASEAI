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

# STAGE 4: NORMALIZATION (0NF -> 1NF -> 2NF -> 3NF Decomposition)
elif st.session_state.active_stage == 4:
    st.subheader("⚡ Stage 4: Database Normalization Process")
    
    st.markdown("""
    Ders projesi standartlarında normalizasyon, verideki fonksiyonel bağımlılıkları analiz ederek tabloları parçalama sürecidir.
    - **1NF:** Tekrarlayan grupları çıkar, atomik değerler sağla.
    - **2NF:** Kısmi bağımlılıkları (Partial Dependency) kaldır.
    - **3NF:** Geçişli bağımlılıkları (Transitive Dependency) kaldır.
    """)

    if st.button("✨ Normalizasyon Sürecini Analiz Et (Ders Formatı)", type="primary"):
        with st.spinner("Tablolar ayrıştırılıyor..."):
            # Prompt: AI'yı her adımda değişen tablo yapılarını ayrı nesneler olarak dönmeye zorluyoruz
            prompt = f"""
            As a Database Professor, decompose the '{domain}' system (Entities: {entities}) from 0NF to 3NF.
            Return ONLY a raw JSON list. Each object represents a table in a specific NF stage.
            Structure:
            [
                {{"Stage": "1NF", "TableName": "...", "Columns": "...", "Key": "PK", "Description": "Atomic conversion"}},
                {{"Stage": "2NF", "TableName": "...", "Columns": "...", "Key": "PK, FK", "Description": "Removed partial dependency"}},
                {{"Stage": "3NF", "TableName": "...", "Columns": "...", "Key": "PK, FK", "Description": "Removed transitive dependency"}}
            ]
            Provide a complete decomposition where 3NF shows the final multi-table schema.
            """
            
            result = call_ai(prompt)
            
            if result:
                st.session_state.norm_data = result
                st.success("Normalizasyon ayrıştırması tamamlandı!")
            else:
                st.error("AI veri formatını oluşturamadı. Lütfen tekrar deneyin.")

    # --- TABLOLARIN GÖSTERİMİ ---
    if 'norm_data' in st.session_state and st.session_state.norm_data:
        norm_list = st.session_state.norm_data
        
        # Her aşama için bir sekme
        tabs = st.tabs(["🔴 1. Normal Form", "🟡 2. Normal Form", "🟢 3. Normal Form"])
        
        stages = ["1NF", "2NF", "3NF"]
        for i, stage in enumerate(stages):
            with tabs[i]:
                st.markdown(f"### {stage} Aşamasındaki Tablo Yapıları")
                
                # İlgili NF aşamasına ait tüm tabloları filtrele
                stage_tables = [t for t in norm_list if t.get("Stage") == stage]
                
                if stage_tables:
                    for table in stage_tables:
                        with st.expander(f"📋 Tablo: {table.get('TableName')}", expanded=True):
                            # Tablo detaylarını göster
                            st.write(f"**Anahtarlar:** `{table.get('Key')}`")
                            st.write(f"**Kolonlar:** {table.get('Columns')}")
                            st.caption(f"*Mantık:* {table.get('Description')}")
                            
                    # Ayrıca tüm aşamayı özet bir tablo olarak göster
                    st.divider()
                    st.dataframe(stage_tables, use_container_width=True)
                else:
                    st.info(f"{stage} analizi için veri bulunamadı.")

# STAGE 5: ER DIAGRAM
elif st.session_state.active_stage == 5:
    st.subheader("🖼️ Stage 5: ER Diagram (Crow’s Foot Notation)")
    
    if st.button("✨ ER Diyagramını Oluştur", type="primary"):
        with st.spinner("Şema görselleştiriliyor..."):
            prompt = f"Generate a Mermaid.js ER diagram using Crow's Foot for: {domain}. Entities: {entities}. Return ONLY raw mermaid code."
            
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            mermaid_code = response.choices[0].message.content.replace("```mermaid", "").replace("```", "").strip()
            
            import base64
            encoded_string = base64.b64encode(mermaid_code.encode('utf-8')).decode('utf-8')
            image_url = f"https://mermaid.ink/img/{encoded_string}"
            
            # --- KÜÇÜLTME MANTIĞI ---
            # Sütun oranlarını [2, 1, 2] yaparak diyagramı merkeze hapsediyoruz ve küçültüyoruz
            col_left, col_mid, col_right = st.columns([1, 1.5, 1])
            
            with col_mid:
                st.markdown("🔍 **Önizleme (Küçültülmüş Görünüm)**")
                # use_container_width=True kalsa bile col_mid dar olduğu için resim küçük görünecektir
                st.image(image_url, use_container_width=True)
                
            st.success("✅ Diyagram başarıyla optimize edildi.")

# STAGE 6: SQL SCRIPT (Tam Senkronizasyon ve Hata Önleyici)
elif st.session_state.active_stage == 6:
    st.subheader("⌨️ Stage 6: SQL Code Generation (PHPMyAdmin Optimized)")
    
    if st.button("✨ ChatGPT ile Tam SQL ve Trigger Betiği Üret"):
        with st.spinner("Tablo yapıları ve kurallar senkronize ediliyor..."):
            # Stage 1 ve Stage 3'ten gelen güncel kuralları ve tablo tanımlarını alıyoruz
            main_rules = str(st.session_state.get('rules_data', []))
            table_definitions = str(st.session_state.get('table_defs', []))
            
            prompt = f"""
            As a Senior Database Architect, generate a full MariaDB/MySQL script for the '{domain}' system.
            Entities: {entities}.
            Actual Table Structure: {table_definitions}.

            STRICT REQUIREMENTS TO PREVENT ERROR 1054:
            1. DOCUMENTATION TABLE: Create a table named `_business_rules` with EXACT columns: 
               `id` (INT PK AUTO_INCREMENT), `rule_id` (VARCHAR(10)), `rule_statement` (TEXT), `logic_type` (VARCHAR(50)), `created_at` (TIMESTAMP).
               - USE 'CREATE TABLE IF NOT EXISTS'.
               - Convert ALL items in this list to INSERT statements: {main_rules}.
            2. NO FAKE COLUMNS: Check {table_definitions}. If a trigger checks a column (like 'total' or 'start_time'), it MUST exist in the table definition. If it doesn't exist, use the closest actual column name or skip the logic.
            3. TRIGGERS: Wrap EACH trigger with 'DELIMITER //' and 'DELIMITER ;'. Use local 'DECLARE' for variables.
            4. FORMAT: Return ONLY the raw SQL code block. No explanations.
            """
            
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            raw_content = response.choices[0].message.content
            
            sql_match = re.search(r"```sql\n(.*?)\n```", raw_content, re.DOTALL)
            clean_sql = sql_match.group(1) if sql_match else raw_content.replace("```sql", "").replace("```", "")
            
            st.session_state.full_sql = clean_sql.strip()
            st.success("✅ SQL Hazır! Sütun isimleri tablo tanımlarıyla senkronize edildi.")

    if 'full_sql' in st.session_state:
        st.code(st.session_state.full_sql, language="sql")

# STAGE 7: DEPLOY (Zeki SQL Yürütücü)
elif st.session_state.active_stage == 7:
    st.subheader("🚀 Final Step: Deployment to PHPMyAdmin")
    safe_db_name = domain.lower().replace(" ", "_") + "_db"
    
    if st.button("🚀 EXECUTE ON MySQL"):
        if 'full_sql' not in st.session_state or not st.session_state.full_sql:
            st.error("Önce 'SQL Script' aşamasında kod üretmelisiniz!")
        else:
            try:
                conn = mysql.connector.connect(host="localhost", user="root", password="")
                cursor = conn.cursor()
                cursor.execute(f"CREATE DATABASE IF NOT EXISTS `{safe_db_name}`")
                cursor.execute(f"USE `{safe_db_name}`")
                cursor.execute("SET FOREIGN_KEY_CHECKS = 0")
                
                import re
                raw_sql = st.session_state.full_sql
                sql_commands = []
                
                # Delimiter bloklarını (Triggerlar) parçalamadan ayıkla
                trigger_pattern = r"DELIMITER //(.*?)\/\/ DELIMITER ;"
                triggers = re.findall(trigger_pattern, raw_sql, re.DOTALL)
                non_trigger_sql = re.sub(trigger_pattern, "", raw_sql, flags=re.DOTALL)
                
                # Normal komutları (CREATE, INSERT) ; ile böl
                for cmd in non_trigger_sql.split(';'):
                    if cmd.strip(): sql_commands.append(cmd.strip())
                
                # Tetikleyicileri tek parça olarak ekle
                for trg in triggers:
                    if trg.strip(): sql_commands.append(trg.strip())

                success_count = 0
                error_logs = []
                for command in sql_commands:
                    upper_cmd = command.upper().strip()
                    if any(upper_cmd.startswith(x) for x in ["CREATE", "INSERT", "ALTER", "DROP", "SET"]):
                        try:
                            cursor.execute(command)
                            success_count += 1
                        except Exception as e:
                            error_logs.append(f"Hata: {str(e)}")
                
                cursor.execute("SET FOREIGN_KEY_CHECKS = 1")
                conn.commit()
                if success_count > 0:
                    st.success(f"✅ {success_count} komut başarıyla aktarıldı!")
                    st.balloons()
                if error_logs:
                    with st.expander("Bazı komutlar işlenemedi (Sütun adı hataları olabilir)"):
                        for log in error_logs: st.warning(log)
                cursor.close()
                conn.close()
            except Exception as e:
                st.error(f"MySQL Bağlantı Hatası: {e}")

if st.session_state.active_stage == 0:
    st.info("Süreci başlatmak için lütfen sol menüdeki tanımları yapın ve Stage 1'e tıklayın.")