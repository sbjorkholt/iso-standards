
###############################################################################################################################
#################################                           CLEANING                             ##############################
###############################################################################################################################

tc_names <- readRDS("../raw_data/archive_members/participants_v1.rds") %>%
  select(committee, title)

liaison_v1 <- read_rds("../raw_data/archive_liaison/liaison_v1_table.rds") %>% 
  rename(name = organization) %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  unnest(cols = c(liaison)) %>%
  mutate(type = NA) %>%
  select(-webid) %>%
  rename(committee = liaison) %>%
  left_join(tc_names, by = join_by(committee)) %>%
  select(acronym, name, address, country, year, committee, title, type)

liaison_v2 <- read_rds("../raw_data/archive_liaison/liaison_v2_table.rds") %>%
  rename(type = Type) %>%
  select(acronym, name, address, country, year, committee, title, type)

liaison_v3 <- read_rds("../raw_data/archive_liaison/liaison_v3_table.rds") %>%
  rename(type = Type) %>%
  select(acronym, name, address, country, year, committee, title, type)

liaison_v4 <- read_rds("../raw_data/archive_liaison/liaison_v4_table.rds") %>%
  rename(type = Type) %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  select(acronym, name, address, country, year, committee, title, type)

liaison_current_2023 <- read_rds("../raw_data/archive_liaison/liaison_current_table.rds") %>%
  rename(type = Type) %>%
  mutate(year = 2023) %>%
  select(acronym, name, address, country, year, committee, title, type)

#### 1. Order data and fix names so that they correspond over time ####

liaison_wayback <- rbind(liaison_v1, # Bind together data from all webpage versions
                         liaison_v2,
                         liaison_v3,
                         liaison_v4,
                         liaison_current_2023) %>%
  mutate(country = ifelse(country == "", NA, country),
         name = ifelse(name == "", NA, name)) %>%
  mutate(country = ifelse(country == "Rica", "Costa Rica",
                          ifelse(country == "of", "Iran",
                                 ifelse(country == "Kong", "Hong Kong",
                                        ifelse(country == "States", "United States",
                                               country))))) %>%
  mutate(acronym = case_when(
    acronym == "ECOS (Europe" ~ "ECOS (Europe)",
    acronym == "INT, France" ~ "INT",
    acronym == "CET - thé" & country == "Germany" ~ "European Tea Committee",
    acronym == "CET - ceramic" & country == "Belgium" ~ "European Ceramic Tile Manufacturers' Federation",
    acronym == "Canada" ~ "ISSPA, Canada",
    acronym == "IFRA - fragrance" ~ "IFRA",
    acronym == "IFA-International Fireworks Association" ~ "IFA - International Fireworks Association",
    acronym == "isda" ~ "ISDA",
    acronym == "WHO" & country == "Denmark" ~ "WHO-ECEH",
    acronym == "iiSBE - iiSBE" ~ "iiSBE",
    acronym == "eCl@ss e.V" ~ "eCl@ss e.V.",
    acronym == "ISC)2" ~ "(ISC)2",
    acronym == "NATO AC 35, United Kingdom" ~ "NATO AC 35",
    acronym == "Opengroup, United Kingdom" ~ "Opengroup",
    acronym == "World Shipping Council" ~ "WSC",
    acronym == "DMSC Inc" ~ "DMSC",
    acronym == "DMSC Inc." ~ "DMSC",
    acronym == "AIB_Association of Issuing Bodies" ~ "AIB",
    acronym == "Global Platform - Global Platform Inc." ~ "Global Platform",
    acronym == "IMS Global ." ~ "IMS Global",
    acronym == "MiA_Marketplace Industry Association, Inc." ~ "MiA",
    acronym == "QuEST Forum" ~ "QuEST",
    name == "ASEAN Cosmetic Association (ACA)" ~ "ACA",
    name == "Advanced Micro devices, Inc. - AMD" ~ "AMD",
    name == "Apiceutical Research Centre / International Propolis Research Group" ~ "ARC/IPRG",
    name == "Association of Issuing Bodies ivzw" ~ "AIB",
    name == "Big Data Value AISBL" ~ "BDVA",
    name == "Biobanking and BioMolecular resources Research Infrastructure" ~ "BBMRI",
    name == "Business Software Alliance (BSA)" ~ "BSA",
    name == "CARICOM Regional Organisation for Standards & Quality (CROSQ)" ~ "CROSQ",
    name == "The Latin American Cosmetics, Toiletry and Perfumery Association" ~ "CASIC",
    name == "CCN, Centro Criptologico Nacional" ~ "CCN",
    name == "CIES - The Food Business Forum" ~ "CGF",
    name == "The Consumer Goods Forum" ~ "CGF",
    name == "CITA – The International Motor Vehicle Inspection Committee" ~ "CITA",
    name == "CONCAWE - The Oil Companies' European Organization for Environmental and Health Protection" ~ "CONCAWE",
    name == "CPM ENGINEERING (PTY) LTD" ~ "CMP",
    name == "Cloud security alliance" ~ "CSA",
    name == "Collaborative International Pesticides Analytical Council Limited (CIPAC)" ~ "CIAPAC",
    name == "Committee of Professional Agircultural Organisations in the European Union (COPA)" ~ "COPA-COGEA",
    name == "Committee of Professional Agricultural Organisations in the European Union (COPA) - General Confederation of Agricultural Co-operative in the European Union (COGECA)" ~ "COPA-COGEA",
    name == "Contant ID Forum (CIDF)" ~ "CIDF",
    name == "Cruise Lines International Association (CLIA)" ~ "CLIA",
    name == "Digital Cinema Common Specification Development Project & Digital Cinema Common Specification Development Committee" ~ "DCI",
    name == "Document, Discover and Interoperate" ~ "DDI",
    name == "Dublin Core Metadata Initiative (DCMI)" ~ "DCMI",
    name == "EUVEPRO European Vegetable Proteins Association" ~ "EUVEPRO",
    name == "Efficiency Valuation Organization (EVO)" ~ "EVO",
    name == "Electronic Book Exchange Working Group (EBX)" ~ "EBX",
    name == "Electronic Commerce Code Management Association (ECCMA)" ~ "ECCMA",
    name == "European Cement Research Academy - ECRA" ~ "ECRA",
    name == "European Facility Management Network (EuroFM)" ~ "EuroFM",
    name == "European Federation of national associations of water & wastewater services" ~ "EurEau",
    name == "European Medical Technology Industry Association (EUCOMED)" ~ "EUCOMED",
    name == "European Association of Craft, Small and Medium-sized Enterprises" ~ "SMEunited",
    name == "European Office of Crafts, Trades and Small and Medium-sized Enterprises for Standardisation" ~ "SMEunited",
    name == "European Petroleum Industry Association (EUROPIA)" ~ "EUROPIA",
    name == "European association of gas and steam turbine manufacturers" ~ "EUTurbines",
    name == "European bedding industries' association" ~ "EBIA",
    name == "European diisocyanates and polyols producers association" ~ "ISOPA",
    name == "European directorate for the quality of medecines & healthcare" ~ "EDQM",
    name == "European Laboratory Medicine" ~ "EFLM",
    name == "European Federation of Clinical Chemistry and Laboratory Medicine" ~ "EFLM",
    name == "European organisation for external Quality Assurance providers in Laboratory Medicine" ~ "EQALM",
    name == "European radiation dosimetry group" ~ "Eurados",
    name == "European, Middlle Eastern and African Society for Biobanking (ESBB)" ~ "ESBB",
    name == "Federation of Oils, Seeds and Fats Associations Ltd (FOSFA International)" ~ "FOSFA",
    name == "Federation of the European Rigid Polyurethane Foam Associations (BING)" ~ "BING",
    name == "Federation of the European Rigid Polyurethane Foam Associations" ~ "BING",
    name == "Global Spatial Data Infrastructure (GSDI) Secretariat" ~ "GSDI",
    name == "Globalization and Localization Association (GALA)" ~ "GALA",
    name == "ICIA" ~ "ICIA",
    name == "IGS - International Geosynthetics Society" ~ "IGS",
    name == "IRF - Geneva" ~ "IRF",
    name == "ISMC" ~ "ISMC",
    name == "Institute of Electrical and Electronics Engineers Computer Society" ~ "IEEE",
    name == "Institute of Electrical and Electronics Engineers, Inc" ~ "IEEE",
    name == "InterAmerican CSR' Network (RSE)" ~ "RSE",
    name == "Interdisciplinary Centre for Law and ICT (ICRI)" ~ "ICRI",
    name == "International Association of Exhibition and Event Services" ~ "IFES",
    name == "International Association of Exhibitions & Events" ~ "IFES",
    name == "International Association of Independent Tanker Owners - INTERTANKO" ~ "INTERTANKO",
    name == "International Association on Coffee Science - ASIC" ~ "ASIC", 
    name == "International Federation for Produce Standards (IFPS)" ~ "IFPS",
    name == "International Federation of Fruit Juice Producers - IFU" ~ "IFU",
    name == "International Information Centre for Terminology (Infoterm)" ~ "Infoterm",
    name == "International Olive Oil Council" ~ "IOC",
    name == "International Olive Council" ~ "IOC",
    name == "International Personnel Certification Association (IPC)" ~ "IPC",
    name == "International Railway Industry Standard (IRIS)" ~ "IRIS",
    name == "International Road Federation - Geneva" ~ "IRF",
    name == "International Sailing Federation (ISAF)" ~ "ISAF",
    name == "International Society for Quality in Health Care (ISQua)" ~ "ISQua",
    name == "Manufacturing Enterprise Solutions Association (MESA)" ~ "MESA",
    name == "NXP B.V." ~ "NXP",
    name == "National Information Standards Organization (NISO)" ~ "NISO",
    name == "North Atlantic Treaty Organisation (AC/35)" ~ "NATO",
    name == "North Atlantic Treaty Organisation (Air Group IV)" ~ "NATO",
    name == "OIPF (the Open IPTV Forum)" ~ "OIPF",
    name == "Organisation for Economic Co-operation and Development, OECD" ~ "OECD",
    name == "Organization for the Advancement of Structured Information Standards (OASIS)" ~ "OASIS",
    name == "Pacific Accreditation Cooperation (PAC)" ~ "PAC",
    name == "Pan American Standards Commission (COPANT)" ~ "COPANT",
    name == "Pan-American Institute of Geography and History (PAIGH)" ~ "PAIGH",
    name == "European Network on Quality of Experience in Multimedia Systems and Services" ~ "QUALINET",
    name == "Rehabilitation International (RI)" ~ "RI",
    name == "Rehabilitation International" ~ "RI",
    name == "Royal Life Saving Society UK (RLSSUK)" ~ "RLSSUK",
    name == "SEmiconductor MAnufacturing TECHnology (SEMATECH)" ~ "SEMATECH",
    name == "Superyacht Builders Association (SYBAss)" ~ "SYBass",
    name == "The Digital TV Group (DTG)" ~ "DTG",
    name == "The European Cosmetic Toiletry and Perfumery Association (COLIPA)" ~ "COLIPA",
    name == "The IT Service Management Forum International Limited (itSMFI)" ~ "itSMFI",
    name == "The Linked Data Benchmark Council (LDBC)" ~ "LDBC",
    name == "The Universal Decimal Classification (UDC)" ~ "UDC",
    name == "Third Generation Partners Project (3GPP)" ~ "3GPP",
    name == "Traveller Information Services Association (TISA)" ~ "TISA",
    name == "UN/ECECEFACT International Trade and Business Processes Group" ~ "UN/CEFACT",
    name == "UN/ECE CEFACT" ~ "UN/CEFACT",
    name == "UN/ECOSOC Sub-Committee of Experts on the transport of Dangerous Goods (TDG)" ~ "UN/ECOSOC",
    name == "UNIFE - The European Railway Industries" ~ "UNIFE",
    name == "Union Internationale des Laboratoires Indépendants (UILI)" ~ "UILI",
    name == "Video services forum, Inc. (VSF)" ~ "VSF",
    name == "Virtual World Forum (VirF)" ~ "VirF",
    name == "World Association for Public Opinion Research (WAPOR)" ~ "WAPOR",
    name == "World Organization for Animal Health (OIE)" ~ "OIE",
    name == "World Sailing (WS)" ~ "WS",
    TRUE ~ acronym)
    ) %>%
  mutate(name = case_when(
    acronym == "WIPO" ~ "World Intellectual Property Organization",
    acronym == "WHO" & country == "Switzerland" ~ "World Health Organization",
    acronym == "WHO-ECEH" ~ "WHO European Centre for Environment and Health",
    acronym == "WPO" ~ "World Packaging Organization",
    acronym == "WASPaLM" ~ "World Association of Societies of Pathology and Laboratory Medicine",
    acronym == "WMO" ~ "World Meteorological Organization",
    acronym == "WFO" ~ "World Foundrymen Organization",
    acronym == "WAPOR" ~ "World Association for Public Opinion Research",
    acronym == "UNSCETDG" ~ "UN/ECOSOC Sub-Committee of Experts on the transport of Dangerous Goods (TDG)",
    acronym == "UNIDO" ~ "United Nations Industrial Development Organization",
    acronym == "AccountAbility" ~ "AccountAbility",
    acronym == "BIAC" ~ "Business at OECD",
    acronym == "CCIB" ~ "Barcelona Convention Bureau",
    acronym == "CIAA" ~ "Confederation of the. Food and Drink Industries of the European Union",
    acronym == "DMSC" ~ "Digitial Metrology Standards Consortium",
    acronym == "EBEN" ~ "Eben Consultants (F.E.) Pte Ltd",
    acronym == "EFAEP" ~ NA,
    acronym == "EIRIS" ~ "Ethical Investment Research and Information Service",
    acronym == "FIDIS" ~ "Future of Identity in the Information Society",
    acronym == "FLA" ~ "Fair Labor Association",
    acronym == "Forum Empresa" ~ "Forum Empresa/Ethos Institute",
    acronym == "IABC" ~ "International Association of Business Communicators",
    acronym == "IDOIF" ~ NA,
    acronym == "IIC" ~ "International Chamber of Commerce",
    acronym == "IOOC/COI" ~ "International Olive Council",
    acronym == "(ISC)2" ~ "International Information Systems Security Certification Consortium, Inc.",
    acronym == "NATO AC 35" ~ "North Atlantic Treaty Organisation (AC/35)",
    acronym == "Opengroup" ~ "Opengroup",
    acronym == "Red Puentes" ~ "Red Puentes",
    acronym == "SAI" ~ "SAI Global Limited",
    acronym == "UN Global Compact" ~ "UN Global Compact",
    acronym == "UNECLAC" ~ "Economic Commission for Latin America and the Caribbean",
    acronym == "WSA" ~ "World Steel Association",
    acronym == "WSC" ~ "World Shipping Council",
    acronym == "EFCO&HPA;" ~ "European Federation of Campingsite Organisations and Holiday Park Associations",
    acronym == "IH&RA;" ~ "International Hotel & Restaurant Association",
    acronym == "QuEST" ~ "Quality Excellence for Suppliers of Telecommunications",
    acronym == "LTAC/TerminOrgs" ~ "Language Terminology/Translation and Authoring Consortium/Terminology for Large Organizations",
    acronym == "The SPICE User Group" ~ "Software Process Improvement and Capability Determination User Group",
    TRUE ~ name)
  ) %>%
  mutate(country = case_when(
    acronym == "WHO-ECEH" ~ "Denmark",
    name == "Uitgebreid Samenwerkingsverband ProcesIndustrie, Nederland" ~ "Netherlands",
    TRUE ~ country 
  )) %>%
  mutate(name = ifelse(acronym == "INT", "Institut National des Télécommunications",
                       ifelse(name == "Confederation of the. Food and Drink Industries of the European Union", "Confederation of the Food and Drink Industries of the European Union",
                              ifelse(name == "Interdisciplinary Centre for Law and ICT (ICRI),", "Interdisciplinary Centre for Law and ICT (ICRI)",
                                     name))),
         address = ifelse(acronym == "INT", "Institut National des Télécommunications9, Rue Charles Fourier 91011 Evry France", address),
         country = ifelse(acronym == "INT", "France", country)) %>%
  mutate(name = case_when(
    name == "European Committee of Manufacturers of Gas- Welding Equipment" ~ "European Committee of Manufacturers of Gas-Welding Equipment",
    name == "AOAC INTERNATIONAL, Association of Analytical Communities" ~ "AOAC International",
    name == "ASEAN Cosmetic Association (ACA)" ~ "ASEAN Cosmetic Association",
    name == "ADA-Europe" ~ "Ada-Europe",
    name == "Advanced Micro devices, Inc. - AMD" ~ "Advanced Micro Devices Inc.",
    name == "AeroSpace and Defence Industries Association of Europe - Standardization" ~ "Aero Space and Defence Industries Association of Europe",
    name == "Association of Issuing Bodies ivzw" ~ "Association of Issuing Bodies",
    name == "Big Data Value AISBL" ~ "Big Data Value Association",
    name == "Biobanking and BioMolecular resources Research Infrastructure" ~ "Biobanking and Biomolecular Resources Research Infrastructure",
    name == "Business Software Alliance (BSA)" ~ "Business Software Alliance",
    name == "CARICOM Regional Organisation for Standards & Quality (CROSQ)" ~ "CARICOM Regional Organisation for Standards & Quality",
    name == "CASIC, The Latin American Cosmetics, Toiletry and Perfumery Association" ~ "The Latin American Cosmetics, Toiletry and Perfumery Association",
    name == "CCN, Centro Criptologico Nacional" ~ "Centro Criptologico Nacional",
    name == "CIES - The Food Business Forum" ~ "The Consumer Goods Forum", # changed name
    name == "CISC Semiconductor Design+Consulting GmbH" ~ "CISC Semiconductor",
    name == "CITA – The International Motor Vehicle Inspection Committee" ~ "The International Motor Vehicle Inspection Committee",
    name == "CONCAWE - The Oil Companies' European Organization for Environmental and Health Protection" ~ "The Oil Companies' European Organization for Environmental and Health Protection",
    name == "CPM ENGINEERING (PTY) LTD" ~ "CMP Engineering Pty Ltd.",
    name == "Cloud security alliance" ~ "Cloud Security Alliance",
    name == "Collaborative International Pesticides Analytical Council Limited (CIPAC)" ~ "Collaborative International Pesticides Analytical Council Limited",
    name == "Committee of Professional Agircultural Organisations in the European Union (COPA)" ~ "Committee of Professional Agricultural Organisations in the European Union (COPA) - General Confederation of Agricultural Co-operative in the European Union (COGECA)",
    name == "Committee of Professional Agricultural Organisations in the European Union (COPA) - General Confederation of Agricultural Co-operative in the European Union (COGECA)" ~ "Committee of Professional Agricultural Organisations in the European Union (COPA) - General Confederation of Agricultural Co-operative in the European Union (COGECA)",
    name == "Committee on Earth Observation Satellites, Working Group on Information Systems and Services" ~ "Committee on Earth Observation Satellites",
    name == "Common LAnguage Resources and Technology INfrastructure" ~ "Common Language Resources and Technology Infrastructure",
    name == "Confederation of the Food and Drink Industries in the EU" ~ "Confederation of the Food and Drink Industries of the European Union",
    name == "Contant ID Forum (CIDF)" ~ "Contant ID Forum",
    name == "Cruise Lines International Association (CLIA)" ~ "Cruise Lines International Association",
    name == "Digital Cinema Common Specification Development Project & Digital Cinema Common Specification Development Committee" ~ "Digital Cinema Inititatives",
    name == "Document, Discover and Interoperate" ~ "Data Documentation Initiative",
    name == "Dublin Core Metadata Initiative (DCMI)" ~ "Dublin Core Metadata Initiative",
    name == "EUVEPRO European Vegetable Proteins Association" ~ "European Vegetable Proteins Association",
    name == "Efficiency Valuation Organization (EVO)" ~ "Efficiency Valuation Organization",
    name == "Electronic Book Exchange Working Group (EBX)" ~ "Electronic Book Exchange Working Group",
    name == "Electronic Commerce Code Management Association (ECCMA)" ~ "Electronic Commerce Code Management Association",
    name == "European Aluminium Association" ~ "European Aluminium",
    name == "European Cement Research Academy - ECRA" ~ "European Cement Research Academy",
    name == "European Commission - Joint Research Centre" ~ "European Commission Joint Research Centre",
    name == "European Commission for Democracy through Law of the Council of Europe" ~ "European Commission for Democracy through Law",
    name == "European Confederation of Wood- Working Industries" ~ "European Confederation of Woodworking Industries",
    name == "European Facility Management Network (EuroFM)" ~ "European Facility Management Network",
    name == 'European Federation of Building Joinery Manufacturers - Sub-Commission "GLULAM"' ~ "European Federation of Building Joinery Manufacturers",
    name == "European Federation of Camping Site Organisations" ~ "European Federation of Campingsite Organisations and Holiday Park Associations",
    name == "European Federation of national associations of water & wastewater services" ~ "European Federation of National Associations of Water Services",
    name == "European Medical Technology Industry Association (EUCOMED)" ~ "European Medical Technology Industry Association",
    name == "European Office of Crafts, Trades and Small and Medium-sized Enterprises for Standardisation" ~ "European Association of Craft, Small and Medium-sized Enterprises",
    name == "European Petroleum Industry Association (EUROPIA)" ~ "European Petroleum Industry Association",
    name == "European Software Institute (Tecnalia Research & Innovation)" ~ "European Software Institute",
    name == "European association of gas and steam turbine manufacturers" ~ "European Association of Gas and Steam Turbine Manufacturers",
    name == "European bedding industries' association" ~ "European Bedding Industries' Association",
    name == "European directorate for the quality of medecines & healthcare" ~ "European Directorate for the Quality of Medicines & HealthCare",
    name == "European Laboratory Medicine" ~ "European Federation of Clinical Chemistry and Laboratory Medicine",
    name == "European organisation for external Quality Assurance providers in Laboratory Medicine" ~ "European Organisation for External Quality Assurance Providers in Laboratory Medicine",
    name == "European radiation dosimetry group" ~ "European Radiation Dosimetry Group",
    name == "European, Middlle Eastern and African Society for Biobanking (ESBB)" ~ "European, Middle Eastern & African Society for Biopreservation and Biobanking",
    name == "FIX Protocol Ltd" ~ "FIX Trading Community",
    name == "FIX Protocol Ltd." ~ "FIX Trading Community",
    name == "Federation of Oils, Seeds and Fats Associations Ltd (FOSFA International)" ~ "Federation of Oils, Seeds and Fats Associations",
    name == "Federation of the European Rigid Polyurethane Foam Associations (BING)" ~ "Federation of the European Rigid Polyurethane Foam Associations",
    name == "G S1 (formerly EAN International)" ~ "GS1",
    name == "GS1 US" ~ "GS1",
    name == "Global Spatial Data Infrastructure (GSDI) Secretariat" ~ "Global Spatial Data Infrastructure Association",
    name == "Globalization and Localization Association (GALA)" ~ "Globalization and Localization Association",
    name == "ICIA" ~ "Indiana Crop Improvement Association",
    name == "IGS - International Geosynthetics Society" ~ "International Geosynthetics Society",
    name == "IPICO South Africa - Pty - Ltd" ~ "IPICO South Africa Pty Ltd.",
    name == "IRF - Geneva" ~ "International Road Federation",
    name == "ISEAL" ~ "ISEAL Alliance",
    name == "ISMC" ~ "The International Soil Modeling Consortium",
    name == "ITAM.ORG" ~ "ITAMOrg",
    name == "Industrial Minerals Association - Europe AISBL" ~ "Industrial Minerals Association",
    name == "Institute of Electrical and Electronics Engineers Computer Society" ~ "Institute of Electrical and Electronics Engineers",
    name == "Institute of Electrical and Electronics Engineers, Inc" ~ "Institute of Electrical and Electronics Engineers",
    name == "InterAmerican CSR' Network (RSE)" ~ "Inter-American conferences on corporate social responsibility",
    name == "Interdisciplinary Centre for Law and ICT (ICRI)" ~ "Interdisciplinary Centre for Law and ICT",
    name == "International Association of Exhibitions & Events" ~ "International Association of Exhibitions and Events",
    name == "International Association of Exhibition and Event Services" ~ "International Association of Exhibition and Events",
    name == "International Association of Independent Tanker Owners - INTERTANKO" ~ "International Association of Independent Tanker Owners",
    name == "International Association of Lighthouse Authority" ~ "International Association of Marine Aids to Navigation and Lighthouse Authorities",
    name == "International Association on Coffee Science - ASIC" ~ "International Association on Coffee Science",
    name == "International Biometric Industry Association" ~ "International Biometrics + Identity Association" ,
    name == "International Federation for Produce Standards (IFPS)" ~ "International Federation for Produce Standards",
    name == "International Federation of Fruit Juice Producers - IFU" ~ "International Federation of Fruit Juice Producers",
    name == "International Hydrographic Bureau" ~ "International Hydrographic Organisation",
    name == "International Information Centre for Terminology (Infoterm)" ~ "International Information Centre for Terminology" ,
    name == "International Olive Oil Council" ~ "International Olive Council",
    name == "International Personnel Certification Association (IPC)" ~ "International Personnel Certification Association",
    name == "International Railway Industry Standard (IRIS)" ~ "International Railway Industry Standard",
    name == "International Road Federation - Geneva" ~ "International Road Federation",
    name == "International Sailing Federation (ISAF)" ~ "International Sailing Federation",
    name == "International Society for Quality in Health Care (ISQua)" ~ "International Society for Quality in Health Care",
    name == "Manufacturing Enterprise Solutions Association (MESA)" ~ "Manufacturing Enterprise Solutions Association",
    name == "Mastercard International" ~ "MasterCard International",
    name == "NEN ¿ 15459 RA" ~ "NEN 15459 RA",
    name == "NXP B.V." ~ "NXP Semiconductors N.V.",
    name == "National Information Standards Organization (NISO)" ~ "National Information Standards Organization",
    name == "OIPF (the Open IPTV Forum)" ~ "Open IPTV Forum",
    name == "Organisation for Economic Co-operation and Development, OECD" ~ "Organisation for Economic Co-operation and Development",
    name == "Organization for the Advancement of Structured Information Standards (OASIS)" ~ "Organization for the Advancement of Structured Information Standards",
    name == "Pacific Accreditation Cooperation (PAC)" ~ "Pacific Accreditation Cooperation",
    name == "Pan American Standards Commission (COPANT)" ~ "Pan American Standards Commission",
    name == "Pan-American Institute of Geography and History (PAIGH)" ~ "Pan-American Institute of Geography and History",
    name == "QUALINET: European Network on Quality of Experience in Multimedia Systems and Services" ~ "European Network on Quality of Experience in Multimedia Systems and Services",
    name == "Rehabilitation International (RI)" ~ "Rehabilitation International",
    name == "Royal Life Saving Society UK (RLSSUK)" ~ "Royal Life Saving Society UK",
    name == "SEmiconductor MAnufacturing TECHnology (SEMATECH)" ~ "SEmiconductor MAnufacturing TECHnology",
    name == "Southern African Development Community Cooperation in Standardization" ~ "Southern African Development Community",
    name == "Superyacht Builders Association (SYBAss)" ~ "Superyacht Builders Association",
    name == "Sustainable forestry initiative" ~ "Sustainable Forestry Initiative",
    name == "The Digital TV Group (DTG)" ~ "The Digital TV Group",
    name == "The European Cosmetic Toiletry and Perfumery Association (COLIPA)" ~ "The European Cosmetic Toiletry and Perfumery Association",
    name == "The IEA Greenhouse Gas R&D; Programme" ~ "The IEA Greenhouse Gas R&D Programme",
    name == "The IT Service Management Forum International Limited (itSMFI)" ~ "The IT Service Management Forum International Limited",
    name == "The Linked Data Benchmark Council (LDBC)" ~ "The Linked Data Benchmark Council",
    name == "The Universal Decimal Classification (UDC)" ~ "The Universal Decimal Classification",
    name == "The World Bank Group" ~ "The World Bank",
    name == "Third Generation Partners Project (3GPP)" ~ "Third Generation Partners Project",
    name == "Traveller Information Services Association (TISA)" ~ "Traveller Information Services Association",
    name == "UN/ECECEFACT International Trade and Business Processes Group" ~ "UN/CEFACT Trade Facilitation and E-business",
    name == "UN/ECE CEFACT" ~ "UN/CEFACT Trade Facilitation and E-business",
    name == "UN/ECOSOC Sub-Committee of Experts on the transport of Dangerous Goods (TDG)" ~ "UN/ECOSOC Sub-Committee of Experts on the transport of Dangerous Goods",
    name == "UNIFE - The European Railway Industries" ~ "The European Railway Industries",
    name == "Uitgebreid Samenwerkingsverband ProcesIndustrie, Nederland" ~ "Uitgebreid Samenwerkingsverband ProcesIndustrie",
    name == "Union Internationale des Laboratoires Indépendants (UILI)" ~ "Union Internationale des Laboratoires Indépendants",
    name == "UN Economic Commission for Africa" ~ "United Nations Economic Commission for Africa",
    name == "UN Economic and Social Council" ~ "United Nations Economic and Social Council",
    name == "UN Global Compact" ~ "United Nations Global Compact",
    name == "UN Office for Disaster Risk Reduction" ~ "United Nations Office for Disaster Risk Reduction",
    name == "Verband der Automobilindustrie e.V. (VDA)/German Association of the Automotive Industry" ~ "German Association of the Automotive Industry",
    name == "Video services forum, Inc. (VSF)" ~ "Video services forum, Inc.",
    name == "Virtual World Forum (VirF)" ~ "Virtual World Forum",
    name == "Visa Europe Services INC" ~ "Visa - Europe",
    name == "World Bank Group" ~ "The World Bank",
    name == "World Organization for Animal Health (OIE)" ~ "World Organization for Animal Health",
    name == "World Sailing (WS)" ~ "World Sailing",
    name == "the European LPG Association" ~ "European LPG Association",
    TRUE ~ name
  )) %>%
  filter(!committee %in% c("ISO/CASCO", "CASCO", "REMCO", "ISO/REMCO")) %>%
  filter(!acronym %in% c("France", "United Kingdom", "USA")) %>%
  mutate(acronym = str_replace_all(acronym, "ō", "o"),
         name = str_replace_all(name, "ō", "o")) %>%
  mutate(committee = str_remove_all(committee, "ISO/")) %>%
  mutate(year = as.numeric(year)) %>%
  unique()%>%
  mutate(committee = str_replace(committee, "^[IEC ]*", ""),
         committee = str_replace(committee, "\\/WG [0-9]+", ""),
         committee = str_replace(committee, "\\/JWG [0-9]+", "")) %>%
  drop_na(name) %>%
  mutate(name = str_remove(name, "the "),
         name = str_remove(name, "The "),
         name = str_remove_all(name, "[[:punct:]]"),
         name = str_squish(name))

rm(liaison_v1, liaison_v2, liaison_v3, liaison_v4, liaison_current_2023)

##### 2. Adding imputations ######

## Adding country of organization based on available data from other webpages
liaison_wayback <- liaison_wayback %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  left_join(liaison_wayback, by = c("country")) %>%
  group_by(acronym) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(country = first(na.omit(country))) %>%
  select(-n)  %>%
  ungroup()

## Adding name of organization based on similiar methods as above
liaison_wayback <- liaison_wayback %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  left_join(liaison_wayback, by = c("name")) %>%
  group_by(acronym, country) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(name = first(na.omit(name))) %>%
  select(-n) %>%
  ungroup()

## Adding imputations for committee

liaison_wayback <- with(liaison_wayback,  liaison_wayback[order(name) , ])

liaison_per_year <- liaison_wayback %>%
  mutate(impute = 0) %>%
  mutate(year = as.numeric(year))

generate_fill <- function(name, start, end, committee) {
  tibble(name = name, 
         year = seq(start, end, by = 1), 
         committee = committee)
}

table(liaison_per_year$type, useNA = "always")

names <- liaison_per_year %>% select(name) %>% pull() %>% unique() 
tcs <- liaison_per_year %>% select(committee) %>% na.omit() %>% pull() %>% unique()

#### Liaison ####

df1 <- list()
df2 <- list()

for (i in 1:length(names)) {
  
  df2 <- list()
  
  for (j in 1:length(tcs)) {
    
    tmp <- liaison_per_year %>%
      filter(name == names[i],
             committee == tcs[j]) 
    
    if(nrow(tmp) > 0){
      
      tmp <- tmp %>%
        arrange(desc(year))
      
      st_year <- tmp %>% mutate(st_year = min(year)) %>% select(st_year) %>% unique() %>% pull()
      end_year <- tmp %>% mutate(end_year = max(year)) %>% select(end_year) %>% unique() %>% pull()
      
      df2[[j]] <- tmp %>%
        complete(year = st_year:end_year) %>%
        mutate(impute = ifelse(is.na(impute), 1, impute)) %>%
        fill(committee, title, acronym, country, name, .direction = "down") %>%
        fill(type, address, .direction = "downup") %>%
        unique()

    }
    
  }
  
  df1[[i]] <- df2
  
  message(str_c("Finished organization ", names[i],"."))
  
}

liaison_imputed <- bind_rows(df1) %>%
  distinct(name, acronym, year, committee, country, type, impute, .keep_all = TRUE)

# saveRDS(liaison_imputed, file = "../raw_data/archive_liaison/liaison_imputed.rds")

#### 3. Removing duplicates ####

NAtest <- liaison_imputed %>%
  filter(is.na(type)) %>%
  mutate(cyc = str_c(name, "_", year, "_", committee))

Atest <- liaison_imputed %>%
  filter(type == "A") %>%
  mutate(cyc = str_c(name, "_", year, "_", committee)) 

Btest <- liaison_imputed %>%
  filter(type == "B") %>%
  mutate(cyc = str_c(name, "_", year, "_", committee))

Ctest <- liaison_imputed %>%
  filter(type == "C") %>%
  mutate(cyc = str_c(name, "_", year, "_", committee)) 

NAtest <- NAtest %>%
  mutate(double = ifelse(cyc %in% Atest$cyc, 1, 0)) %>% 
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% 
  filter(skip != 1) %>% 
  select(-skip, -double)

NAtest <- NAtest %>%
  mutate(double = ifelse(cyc %in% Btest$cyc, 1, 0)) %>% 
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% 
  filter(skip != 1) %>% 
  select(-skip, -double)

NAtest <- NAtest %>%
  mutate(double = ifelse(cyc %in% Ctest$cyc, 1, 0)) %>% 
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% 
  filter(skip != 1) %>% 
  select(-skip, -double)

liaison <- bind_rows(Atest, Btest, Ctest, NAtest) %>% 
  distinct(year, acronym, name, country, address, committee, title, .keep_all = TRUE)

# saveRDS(liaison, file = "../raw_data/archive_liaison/liaison_tmp.rds")


###### 4. Classification of organizations ########

# source("./4_GPT_coding.R")


###### 5. Adding together #######

# liaison <- liaison %>%
#   left_join(category_justification, by = join_by(acronym, country), relationship = "many-to-many") %>%
#   rename(impute = impute_year_1) %>%
#   left_join(categories %>% rename(category_text = category, category = category_number), by = join_by(category)) %>%
#   select(acronym, name, year, country, category, category_text, description, justification, committee, title, type, impute) %>%
#   distinct()

## Sectors 
sectors <- readRDS("../datasets/sectors.rds") %>%
  unnest() %>%
  ungroup() %>%
  mutate(committee = str_remove_all(committee, "ISO/"),
         committee = str_remove_all(committee, "IEC "))

liaison <- liaison %>%
  left_join(sectors, by = join_by("committee"), relationship = "many-to-many")

na_sectors <- liaison %>%
  distinct() %>% 
  filter(is.na(sector))

na_sectors_filled <- na_sectors %>% 
  select(-sector) %>%
  mutate(committee2 = str_remove(committee, "\\/SC.*")) %>%
  left_join(sectors, by = c("committee2" = "committee")) %>%
  select(-committee2)

liaison <- liaison %>%
  anti_join(na_sectors, by = join_by(committee, title)) %>%
  bind_rows(na_sectors_filled) %>%
  select(-cyc)

saveRDS(liaison, file = "../datasets/liaison.rds")

