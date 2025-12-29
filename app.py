import streamlit as st
import openai
import os
import json
import re
import mysql.connector
import time
from dotenv import load_dotenv
from streamlit_mermaid import st_mermaid

# --- 1. CONFIGURATION ---
load_dotenv()
api_key = os.getenv("OPENAI_API_KEY")
client = openai.OpenAI(api_key=api_key)

st.set_page_config(page_title="Modular Database Architect AI", layout="wide")

# --- 2. CSS DESIGN ---
st.markdown("""
    <style>
    .stApp { background-color: #0d1117; color: #c9d1d9; }
    [data-testid="stSidebar"] { background-color: #010409; border-right: 1px solid #30363d; }
    .metric-card { background: #161b22; border: 1px solid #30363d; border-radius: 8px; padding: 15px; text-align: center; }
    div.stButton > button { width: 100%; border-radius: 6px; height: 48px; font-weight: 600; background-color: #21262d; color: #c9d1d9; border: 1px solid #30363d; }
    div.stButton > button[kind="primary"] { background: #1f6feb !important; border: none !important; color: white !important; }
    </style>
    """, unsafe_allow_html=True)

# --- 3. SCENARIOS ---
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
    if st.button("🚨 DELETE ENTIRE DATABASE"):
        try:
            conn = mysql.connector.connect(host="localhost", user="root", password="")
            cursor = conn.cursor()
            cursor.execute(f"DROP DATABASE IF EXISTS `{temp_db_name}`")
            st.sidebar.warning(f"💥 Database `{temp_db_name}` has been deleted.")
            st.session_state.active_stage = 0
            time.sleep(0.5)
            st.rerun()
        except Exception as e:
            st.sidebar.error(f"Connection Error: {e}")

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
    if st.button("✨ Generate Rules with ChatGPT"):
        with st.spinner("Analyzing..."):
            prompt = f"Extract business rules for {domain}. Entities: {entities}. Rules: {constraints}. Return JSON list: BR-ID, Type (S,O,T,Y), Rule Statement, ER Component (E,R,A,C), Implementation Tip, Rationale."
            st.session_state.rules_data = call_ai(prompt)
    if st.session_state.rules_data: st.table(st.session_state.rules_data)

# STAGE 2: TABLE DEFINITIONS (Business Rule Senkronizasyonu)
elif st.session_state.active_stage == 2:
    st.subheader("📐 Stage 2: Table Definition (Based on Final Business Rules)")
    
    # Mevcut kuralları kontrol et
    if not st.session_state.rules_data:
        st.warning("⚠️ Warning: Your Business Rules list is empty. Please generate and fix rules in Stage 1 & 3 first for better table design.")
    
    if st.button("✨ Generate Table Schemas from Final Rules"):
        with st.spinner("Designing tables to support all business rules..."):
            # En güncel, fixlenmiş kuralları AI'ya gönderiyoruz
            final_rules_json = str(st.session_state.rules_data)
            
            prompt = f"""
            As a Database Architect, design a schema for the '{domain}' system.
            
            STRICT REQUIREMENT: 
            The tables MUST support all logic defined in these Business Rules: {final_rules_json}.
            
            Example: If a rule says 'Max 3 books per student', ensure there is a relationship between Students and Loans.
            Example: If a rule says 'Book status must be tracked', add a 'status' column to the 'Books' table.

            Return a JSON list of objects. Each object MUST have:
            'table_name', 
            'columns' (list of objects with 'name', 'type', 'constraints'),
            'supported_rules' (list of BR-IDs this table helps implement).
            """
            st.session_state.table_defs = call_ai(prompt)

    if 'table_defs' in st.session_state and st.session_state.table_defs:
        for table in st.session_state.table_defs:
            # Tablo ismini ve hangi kuralları desteklediğini başlıkta göster
            supported = ", ".join(table.get('supported_rules', []))
            with st.expander(f"📂 Table: {table['table_name'].upper()} (Supports: {supported})", expanded=True):
                cols_data = table['columns'] 
                st.table(cols_data)
                
                if 'relationships' in table:
                    st.caption(f"🔗 **Relationships:** {table['relationships']}")
    else:
        st.info("Click the button to generate schemas that match your fixed business rules.")

# STAGE 3: AUDIT, FIX & MISSING RULES (ID Takibi ile Denetim)
elif st.session_state.active_stage == 3:
    st.subheader("🔍 Stage 3: Database Audit & Missing Rules Analysis")
    
    col1, col2 = st.columns(2)
    
    with col1:
        if st.button("🔍 Step 1: Audit Current Rules & Scan Gaps"):
            with st.spinner("Auditing your current rules for logical errors and gaps..."):
                # Mevcut kuralları AI'ya gönderiyoruz
                current_rules_json = str(st.session_state.rules_data)
                
                prompt = f"""
                As a Senior Database Auditor, analyze these rules for '{domain}'.
                
                CURRENT RULES: {current_rules_json}

                YOUR TASK:
                1. If a rule has an error or needs improvement, identify it by its 'BR-ID'.
                2. If a critical rule is missing, label it as 'NEW'.
                3. Check for technical feasibility and redundant logic.

                Return ONLY a JSON list with these keys: 
                'Related BR' (BR-ID or NEW), 'Type' (Correction or Missing), 'Finding', 'Severity' (High/Medium), 'Proposed Solution'.
                """
                st.session_state.missing_data = call_ai(prompt)
                
        if st.session_state.missing_data:
            st.markdown("### 📋 Audit Findings & Missing Rules")
            st.table(st.session_state.missing_data)

    with col2:
        if st.session_state.missing_data:
            st.info("💡 Instructions: Corrections will update existing BRs, and 'NEW' items will be added as new BRs.")
            if st.button("🔧 Step 2: Apply Fixes & Integrate to Business Rules"):
                with st.spinner("Processing updates..."):
                    current_rules_text = str(st.session_state.rules_data)
                    audit_findings_text = str(st.session_state.missing_data)
                    
                    prompt = f"""
                    Merge the audit findings into the main business rules list for '{domain}'.
                    
                    ORIGINAL LIST: {current_rules_text}
                    AUDIT FINDINGS/ADDITIONS: {audit_findings_text}

                    LOGIC:
                    - If 'Related BR' matches an existing ID, replace that rule with the 'Proposed Solution'.
                    - If 'Related BR' is 'NEW', add it as a new entry.
                    - Re-index all BR-IDs to be sequential (BR-001, BR-002, etc.).
                    
                    Return a SINGLE JSON list: BR-ID, Type (S,O,T,Y), Rule Statement, ER Component (E,R,A,C), Implementation Tip, Rationale.
                    """
                    updated_rules = call_ai(prompt)
                    if updated_rules:
                        st.session_state.rules_data = updated_rules
                        st.success("✅ Rules updated and new rules appended successfully!")
                        st.balloons()
                        st.rerun()
        else:
            st.info("Run the 'Audit' (Step 1) to analyze your rules.")

    if st.session_state.rules_data:
        with st.expander("📝 Preview Final Optimized Business Rules"):
            st.table(st.session_state.rules_data) #

# STAGE 4: NORMALIZATION (PHPMyAdmin Raporlama Entegrasyonlu)
elif st.session_state.active_stage == 4:
    st.subheader("⚡ Stage 4: Database Normalization (Academic Decomposition)")
    
    st.info("""
    **Normalizasyon Raporlama Mantığı:**
    Bu aşamada yapılan analizler, Stage 6'da SQL olarak üretilecek ve PHPMyAdmin'de `_normalization_log` tablosuna kaydedilecektir. 
    Bu sayede hocanız veritabanını açtığında normalizasyon adımlarınızı görebilecek.
    """)

    if st.button("✨ Perform Academic Normalization & Log Analysis", type="primary"):
        with st.spinner("Decomposing tables and creating audit logs..."):
            # Mevcut tablo tanımlarını ve kuralları referans alarak analiz yaptırıyoruz
            current_tables = str(st.session_state.get('table_defs', []))
            
            prompt = f"""
            As a Database Professor, perform a step-by-step normalization for: '{domain}'.
            Current Table Context: {current_tables}.

            Analyze from 0NF to 3NF. 
            Return ONLY a raw JSON list of objects. Each object represents a step in the process.
            
            Structure:
            [
                {{
                    "NF_Stage": "1NF", 
                    "Target_Table": "Table Name", 
                    "Columns_Included": "list columns", 
                    "Primary_Key": "PK",
                    "Reasoning": "Why this is 1NF (e.g., ensuring atomicity)"
                }},
                {{
                    "NF_Stage": "2NF", 
                    "Target_Table": "Split Table Name", 
                    "Columns_Included": "list columns", 
                    "Primary_Key": "PK",
                    "Reasoning": "Removed partial functional dependencies"
                }},
                {{
                    "NF_Stage": "3NF", 
                    "Target_Table": "Final Table Name", 
                    "Columns_Included": "list columns", 
                    "Primary_Key": "PK",
                    "Reasoning": "Removed transitive dependencies"
                }}
            ]
            """
            normalization_results = call_ai(prompt)
            
            if normalization_results:
                # Sonuçları SQL'e aktarılmak üzere session_state'e kaydediyoruz
                st.session_state.norm_data = normalization_results
                st.success("✅ Normalization analysis complete and ready for SQL export!")

    # --- GÖRSELLEŞTİRME (User Interface) ---
    if 'norm_data' in st.session_state and st.session_state.norm_data:
        res = st.session_state.norm_data
        
        # Sekmeler oluştur
        tab1, tab2, tab3 = st.tabs(["🔴 1st Normal Form", "🟡 2nd Normal Form", "🟢 3rd Normal Form"])
        
        stages = {"1NF": tab1, "2NF": tab2, "3NF": tab3}
        
        for stage_key, tab in stages.items():
            with tab:
                st.markdown(f"### {stage_key} Decomposition")
                # İlgili NF aşamasına ait verileri filtrele
                stage_tables = [t for t in res if t.get("NF_Stage") == stage_key]
                
                if stage_tables:
                    for table in stage_tables:
                        with st.expander(f"📋 Table: {table.get('Target_Table')}", expanded=True):
                            c1, c2 = st.columns([2, 1])
                            with c1:
                                st.write(f"**Columns:** `{table.get('Columns_Included')}`")
                                st.info(f"**Logic:** {table.get('Reasoning')}")
                            with c2:
                                st.markdown(f"🔑 **PK:** `{table.get('Primary_Key')}`")
                else:
                    st.warning(f"{stage_key} için ayrıştırma verisi üretilemedi.")
        
        st.divider()
        st.caption("⚠️ **Not:** Bu adımlar Stage 6'da `_normalization_log` tablosu olarak SQL'e eklenecektir.")
    else:
        st.info("Lütfen normalizasyon analizini başlatmak için yukarıdaki butona tıklayın.")

# STAGE 5: ER DIAGRAM
elif st.session_state.active_stage == 5:
    st.subheader("🖼️ Stage 5: ER Diagram (Crow’s Foot Notation)")
    
    if st.button("✨ Create ER Diagram", type="primary"):
        with st.spinner("Visualizing schema..."):
            prompt = f"Generate a Mermaid.js ER diagram using Crow's Foot for: {domain}. Entities: {entities}. Return ONLY raw mermaid code."
            
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            mermaid_code = response.choices[0].message.content.replace("```mermaid", "").replace("```", "").strip()
            
            import base64
            encoded_string = base64.b64encode(mermaid_code.encode('utf-8')).decode('utf-8')
            image_url = f"https://mermaid.ink/img/{encoded_string}"
            
            # --- KÜÇÜLTME MANTIĞI ---
            col_left, col_mid, col_right = st.columns([1, 1.5, 1])
            
            with col_mid:
                st.markdown("🔍 **Preview (Scaled View)**")
                st.image(image_url, use_container_width=True)
                
            st.success("✅ Diagram successfully optimized.")

# STAGE 6: SQL SCRIPT (Double Documentation Mode: Business & Missing Rules)
elif st.session_state.active_stage == 6:
    st.subheader("⌨️ Stage 6: SQL Code Generation (Full Documentation Mode)")
    
    if st.button("✨ Generate Dual Rule Tables & SQL Script"):
        with st.spinner("Analyzing rules and generating dual documentation tables..."):
            # 1. Verileri Hazırla
            main_rules = str(st.session_state.get('rules_data', [])) # En güncel liste (merge edilmiş)
            missing_only = str(st.session_state.get('missing_data', [])) # Sadece 'Audit' sonucu bulunanlar
            
            prompt = f"""
            As a Senior Database Architect, generate a full MySQL/MariaDB script for '{domain}'.
            Entities: {entities}.
            
            STRICT ACADEMIC DOCUMENTATION REQUIREMENTS:
            1. TABLE 1 (`_business_rules`): Use 'CREATE TABLE IF NOT EXISTS'. Insert ALL current merged rules from {main_rules}.
               Columns: id (PK), rule_id, rule_description, logic_type.

            2. TABLE 2 (`_missing_rules_report`): Create a SEPARATE table to show the gaps you identified.
               Columns: id (PK), related_br, finding, severity, proposed_fix.
               Insert ONLY the findings from this audit list: {missing_only}.
               - This table serves as an Audit Log for the professor to see what was improved.

            3. DATABASE LOGIC:
               - Use 'CREATE TABLE IF NOT EXISTS' for all business entities (Products, Orders, etc.).
               - Write 'BEFORE INSERT' triggers for the rules. Use 'DECLARE' for variables to avoid MariaDB errors.
               - Wrap triggers with 'DELIMITER //' and 'DELIMITER ;'.

            4. FORMAT: Return ONLY raw SQL code. No explanations.
            """
            
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            raw_content = response.choices[0].message.content
            
            # SQL temizliği
            sql_match = re.search(r"```sql\n(.*?)\n```", raw_content, re.DOTALL)
            clean_sql = sql_match.group(1) if sql_match else raw_content.replace("```sql", "").replace("```", "")
            
            st.session_state.full_sql = clean_sql.strip()
            st.success("✅ Dual Documentation Tables generated! Check Stage 7 to Deploy.")

    if 'full_sql' in st.session_state:
        st.info("💡 **Academic Note:** PHPMyAdmin will now show two documentation tables: one for the final rules and one for the audit findings.")
        st.code(st.session_state.full_sql, language="sql")
        st.download_button("📄 Download SQL with Audit Logs", st.session_state.full_sql, file_name="full_audit_db.sql")

# STAGE 7: DEPLOY (Zeki SQL Yürütücü)
elif st.session_state.active_stage == 7:
    st.subheader("🚀 Final Step: Deployment to PHPMyAdmin")
    safe_db_name = domain.lower().replace(" ", "_") + "_db"
    
    if st.button("🚀 EXECUTE ON MySQL"):
        if 'full_sql' not in st.session_state or not st.session_state.full_sql:
            st.error("You must generate code in the 'SQL Script' stage first!")
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
                            error_logs.append(f"Error: {str(e)}")
                
                cursor.execute("SET FOREIGN_KEY_CHECKS = 1")
                conn.commit()
                if success_count > 0:
                    st.success(f"✅ {success_count} commands successfully deployed!")
                    st.balloons()
                if error_logs:
                    with st.expander("Some commands could not be processed"):
                        for log in error_logs: st.warning(log)
                cursor.close()
                conn.close()
            except Exception as e:
                st.error(f"MySQL Connection Error: {e}")

if st.session_state.active_stage == 0:
    st.info("To start the process, please complete the definitions in the left menu and click Stage 1.")