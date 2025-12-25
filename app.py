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

# STAGE 4: NORMALIZATION
elif st.session_state.active_stage == 4:
    st.subheader("⚡ Stage 5: Normalization (0NF → 3NF)")
    st.info("Veritabanı tasarımı, veri tekrarını önlemek için 3. Normal Form seviyesine getiriliyor.")
    
    n_tabs = st.tabs(["1NF (Atomic)", "2NF (Partial Dep)", "3NF (Transitive Dep)"])
    
    with n_tabs[0]:
        st.markdown("#### 1NF: First Normal Form")
        st.write("**Kural:** Çoklu değer içeren sütunlar ve tekrarlayan gruplar kaldırılır. Tüm değerler atomik olmalıdır.")
        
        # Dinamik örnekleme
        main_entity = entities.split(',')[0].strip().upper()
        st.code(f"""
-- 0NF (Hatalı Yapı):
{main_entity} (ID, Name, PhoneNumbers) -- 'PhoneNumbers' birden fazla numara içeriyor.

-- 1NF (Düzeltilmiş Yapı):
{main_entity} (ID, Name)
{main_entity}_PHONES (ID, PhoneNumber) -- Her satırda tek bir telefon numarası.
        """, language="sql")
        st.success("✅ Veriler atomik hale getirildi, tekrarlayan gruplar temizlendi.")

    with n_tabs[1]:
        st.markdown("#### 2NF: Second Normal Form")
        st.write("**Kural:** Tablo 1NF'de olmalı ve birincil anahtarın (PK) bir parçasına bağımlı olan (kısmi bağımlılık) sütunlar kaldırılmalıdır.")
        
        st.code(f"""
-- 1NF (Kısmi Bağımlılık):
ORDER_ITEMS (OrderID, ProductID, OrderDate, Price)
-- 'OrderDate' sadece 'OrderID'ye bağlıdır, PK'nın tamamına değil.

-- 2NF (Düzeltilmiş):
ORDERS (OrderID, OrderDate)
ORDER_ITEMS (OrderID, ProductID, Price)
        """, language="sql")
        st.success("✅ Kısmi fonksiyonel bağımlılıklar giderildi.")

    with n_tabs[2]:
        st.markdown("#### 3NF: Third Normal Form")
        st.write("**Kural:** Tablo 2NF'de olmalı ve anahtar olmayan sütunlar arasındaki geçişli bağımlılıklar (transitive dependencies) kaldırılmalıdır.")
        
        st.code(f"""
-- 2NF (Geçişli Bağımlılık):
STUDENTS (StudentID, Name, DeptID, DeptName)
-- 'DeptName', PK olmayan 'DeptID'ye bağlıdır.

-- 3NF (Düzeltilmiş):
STUDENTS (StudentID, Name, DeptID)
DEPARTMENTS (DeptID, DeptName)
        """, language="sql")
        st.success("✅ Geçişli bağımlılıklar kaldırılarak 3NF seviyesine ulaşıldı.")

# STAGE 5: ER DIAGRAM (Crow’s Foot Notation)
elif st.session_state.active_stage == 5:
    st.subheader("🖼️ Stage 6: ER Diagram (Crow’s Foot Notation)")
    st.markdown("Varlıklar arası kardinaliteler (1:1, 1:N, M:N)")

    if st.button("✨ ER Diyagramını Oluştur"):
        with st.spinner("Şema analiz ediliyor..."):
            # ChatGPT'den dökümana uygun Crow's Foot kodu istiyoruz
            prompt = f"""
            Generate a Mermaid.js ER diagram using Crow's Foot notation for:
            Domain: {domain}
            Entities: {entities}
            Format: erDiagram syntax only.
            Example: STUDENT ||--o{{ LOAN : places
            Return ONLY the mermaid code block.
            """
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            mermaid_code = response.choices[0].message.content.replace("```mermaid", "").replace("```", "").strip()
            
            # Kırık resim hatasını (base64 ile) kesin çözen bölüm:
            import base64
            # Kodu base64 formatına çevirerek URL limitlerini ve karakter hatalarını aşıyoruz
            encoded_string = base64.b64encode(mermaid_code.encode('utf-8')).decode('utf-8')
            mermaid_url = f"https://mermaid.ink/img/{encoded_string}"
            
            # Resmi ekrana basıyoruz
            st.image(mermaid_url, caption=f"{domain} - ER Diagram", use_container_width=True)
            
            st.success("ER Diyagramı başarıyla oluşturuldu!")
            with st.expander("Diyagram Kodunu Gör (Rapor İçin)"):
                st.code(mermaid_code)

# STAGE 6: SQL SCRIPT GENERATION
elif st.session_state.active_stage == 6:
    st.subheader("⌨️ Stage 7: SQL Code Generation")
    st.info("PHPMyAdmin için tam uyumlu SQL betiği hazırlanıyor...")
    
    if st.button("✨ ChatGPT ile Tam SQL Betiği Üret"):
        with st.spinner("Veritabanı mimarisi SQL'e dönüştürülüyor..."):
            prompt = f"""
            As a Senior DBA, generate a full MySQL script for PHPMyAdmin based on:
            Domain: {domain}, Entities: {entities}, Advanced Feature: {adv_feat}.
            
            The script must include:
            1. CREATE TABLE statements with appropriate PK, FK and Data Types.
            2. At least one complex TRIGGER for the advanced feature: {adv_feat}.
            3. INSERT statements with sample data for each table.
            4. Three specific SELECT queries for the requirement: {reporting}.
            
            Use backticks for table names. Ensure Foreign Key constraints are correctly mapped.
            Return ONLY the SQL code.
            """
            response = client.chat.completions.create(model="gpt-4o", messages=[{"role": "user", "content": prompt}])
            st.session_state.full_sql = response.choices[0].message.content.replace("```sql", "").replace("```", "").strip()

    if 'full_sql' in st.session_state:
        st.code(st.session_state.full_sql, language="sql")
        
        st.download_button(
            label="📄 SQL Dosyasını İndir",
            data=st.session_state.full_sql,
            file_name=f"{domain.lower().replace(' ', '_')}_schema.sql",
            mime="text/sql"
        )
        st.success("✅ Kod hazır! Bu kodu PHPMyAdmin > SQL sekmesine yapıştırabilirsiniz.")
    else:
        st.warning("SQL kodunu üretmek için yukarıdaki butona tıklayın.")

# STAGE 7: FINAL DEPLOYMENT (XAMPP / MySQL)
elif st.session_state.active_stage == 7:
    st.subheader("🚀 Final Step: Deployment to PHPMyAdmin")
    
    # Veritabanı ismini standartlaştır (Küçük harf ve alt tire)
    db_name = domain.lower().replace(" ", "_").replace("-", "_") + "_db"
    st.info(f"Bağlantı: **localhost** | Hedef Veritabanı: `{db_name}`")
    
    # Önce SQL üretildi mi kontrol et
    if 'full_sql' not in st.session_state or not st.session_state.full_sql:
        st.warning("⚠️ Lütfen önce 'SQL Script' aşamasında kodları üretin.")
    else:
        if st.button("🚀 TÜM ŞEMAYI PHPMYADMIN'E AKTAR"):
            try:
                # 1. XAMPP MySQL Bağlantısı
                conn = mysql.connector.connect(
                    host="localhost",
                    user="root",
                    password=""
                )
                cursor = conn.cursor()
                
                # 2. Veritabanı Oluştur ve Seç
                cursor.execute(f"CREATE DATABASE IF NOT EXISTS `{db_name}`")
                cursor.execute(f"USE `{db_name}`")
                
                # 3. SQL Betiğini Parçalara Ayır (Tabloları sırayla oluşturmak için)
                # Not: PHPMyAdmin çoklu komutları (multitask) destekler ancak 
                # connector-python için parçalamak daha güvenlidir.
                sql_commands = st.session_state.full_sql.split(';')
                
                progress_bar = st.progress(0)
                status_text = st.empty()
                
                success_count = 0
                for i, command in enumerate(sql_commands):
                    clean_command = command.strip()
                    if clean_command:
                        try:
                            cursor.execute(clean_command)
                            success_count += 1
                        except Exception as e:
                            st.error(f"Komut Hatası: {e}\nKod: `{clean_command[:50]}...`")
                    
                    # İlerleme çubuğu güncelle
                    progress = (i + 1) / len(sql_commands)
                    progress_bar.progress(progress)
                
                conn.commit()
                st.success(f"✅ İşlem Tamamlandı! {success_count} SQL komutu başarıyla çalıştırıldı.")
                st.balloons()
                
                # PHPMyAdmin linkini göster
                st.markdown(f"👉 [PHPMyAdmin'e Git](http://localhost/phpmyadmin/index.php?route=/database/structure&db={db_name})")
                
                cursor.close()
                conn.close()
                
            except Exception as e:
                st.error(f"❌ XAMPP Hatası: {e}")

if st.session_state.active_stage == 0:
    st.info("Süreci başlatmak için lütfen sol menüdeki tanımları yapın ve Stage 1'e tıklayın.")