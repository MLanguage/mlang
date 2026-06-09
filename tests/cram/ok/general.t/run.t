Graph Deps
  $ mlang graph_deps.m --mpp_function target -A app --run_test graph_deps.irj --no_nondet_display --trace --trace_output_file output.json
  [WARNING] Variable "X" définie plus d'une fois dans la même règle
  
    --> graph_deps.m
     | 
  27 | X = 0 + 1 + 2 + FLOAT - FLOAT + VARTMP;
     | ^
  
    --> graph_deps.m
     | 
  36 |   X = X+I;
     |   ^
  
  [WARNING] Auto-cycle dans la règle 1337 avec la variable "X"
  [WARNING] Auto-cycle dans la règle 1337 avec la variable "X"
  [ERROR] KO | X attendue : 3 - evaluée : 58
  [RESULT] graph_deps.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!

  $ cat output.json
  {"kind": "matrix", "graph":{
   "29": {"outcoming": []
  , "incoming": [30]
  },"26": {"outcoming": [24,25]
  , "incoming": [28]
  },"28": {"outcoming": [26,27]
  , "incoming": [30]
  },"12": {"outcoming": []
  , "incoming": [13,14]
  },"1": {"outcoming": []
  , "incoming": [13]
  },"24": {"outcoming": [22,23]
  , "incoming": [26]
  },"32": {"outcoming": [30,31]
  , "incoming": [34]
  },"36": {"outcoming": [34,35]
  , "incoming": [38]
  },"18": {"outcoming": [17]
  , "incoming": []
  },"19": {"outcoming": []
  , "incoming": [20]
  },"22": {"outcoming": [20,21]
  , "incoming": [24]
  },"35": {"outcoming": []
  , "incoming": [36]
  },"23": {"outcoming": []
  , "incoming": [24]
  },"6": {"outcoming": []
  , "incoming": [7]
  },"21": {"outcoming": []
  , "incoming": [22]
  },"0": {"outcoming": []
  , "incoming": [15]
  },"15": {"outcoming": [0,8,14]
  , "incoming": []
  },"14": {"outcoming": [12,13]
  , "incoming": [15]
  },"5": {"outcoming": [4]
  , "incoming": [7,11]
  },"27": {"outcoming": []
  , "incoming": [28]
  },"17": {"outcoming": []
  , "incoming": [18]
  },"7": {"outcoming": [5,6]
  , "incoming": []
  },"20": {"outcoming": [11,19]
  , "incoming": [22]
  },"33": {"outcoming": []
  , "incoming": [34]
  },"11": {"outcoming": [5,10]
  , "incoming": [13,16,20]
  },"9": {"outcoming": [8]
  , "incoming": []
  },"39": {"outcoming": []
  , "incoming": [40]
  },"37": {"outcoming": []
  , "incoming": [38]
  },"40": {"outcoming": [38,39]
  , "incoming": []
  },"34": {"outcoming": [32,33]
  , "incoming": [36]
  },"4": {"outcoming": []
  , "incoming": [5]
  },"16": {"outcoming": [11,13]
  , "incoming": []
  },"8": {"outcoming": []
  , "incoming": [9,15]
  },"10": {"outcoming": []
  , "incoming": [11]
  },"25": {"outcoming": []
  , "incoming": [26]
  },"31": {"outcoming": []
  , "incoming": [32]
  },"13": {"outcoming": [1,11,12]
  , "incoming": [14,16]
  },"30": {"outcoming": [28,29]
  , "incoming": [32]
  },"38": {"outcoming": [36,37]
  , "incoming": [40]
  }},
  "statics": {
  
  "2813454": {"name": "INPUT_DEFINED", "is_input": true, "decl": {"code_orig": "input", "file": "graph_deps.m", "sline": 16, "eline": 16 }, "descr": "blabla", {"code_orig": "input", "file": "graph_deps.m", "sline": 16, "eline": 16 }},
  "59860492": {"name": "ANNEE", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 8, "eline": 8 }, "descr": "annee", {"code_orig": "1337", "file": "graph_deps.m", "sline": 30, "eline": 30 }},
  "71606114": {"name": "V_ANCSDED", "is_input": true, "decl": {"code_orig": "input", "file": "graph_deps.m", "sline": 3, "eline": 3 }, "descr": "v_ancsed", {"code_orig": "input", "file": "graph_deps.m", "sline": 3, "eline": 3 }},
  "82991271": {"name": "VARTMP", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 10, "eline": 10 }, "descr": "vartmp", {"code_orig": "1337", "file": "graph_deps.m", "sline": 23, "eline": 23 }},
  "95966794": {"name": "X", "is_input": true, "decl": {"code_orig": "input", "file": "graph_deps.m", "sline": 4, "eline": 4 }, "descr": "x", {"code_orig": "input", "file": "graph_deps.m", "sline": 4, "eline": 4 }},
  "141781586": {"name": "TAB[0]", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 18, "eline": 18 }, "descr": "tableau", {"code_orig": "1337", "file": "graph_deps.m", "sline": 24, "eline": 24 }},
  "185112339": {"name": "Z", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 9, "eline": 9 }, "descr": "z", {"code_orig": "1337", "file": "graph_deps.m", "sline": 25, "eline": 25 }},
  "259935139": {"name": "TXMARJ", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 7, "eline": 7 }, "descr": "tx_marj", {"code_orig": "1337", "file": "graph_deps.m", "sline": 29, "eline": 29 }},
  "319850048": {"name": "MULTILINE", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 6, "eline": 6 }, "descr": "multiline", {"code_orig": "1337", "file": "graph_deps.m", "sline": 31, "eline": 32 }},
  "328171158": {"name": "X", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 4, "eline": 4 }, "descr": "x", {"code_orig": "1337", "file": "graph_deps.m", "sline": 27, "eline": 27 }},
  "478370311": {"name": "I", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 35, "eline": 35 },  {"code_orig": "1337", "file": "graph_deps.m", "sline": 35, "eline": 35 }},
  "661276726": {"name": "INPUT_Y", "is_input": true, "decl": {"code_orig": "input", "file": "graph_deps.m", "sline": 17, "eline": 17 }, "descr": "blabla", {"code_orig": "input", "file": "graph_deps.m", "sline": 17, "eline": 17 }},
  "731131379": {"name": "VARTMP", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 10, "eline": 10 }, "descr": "vartmp", {"code_orig": "1337", "file": "graph_deps.m", "sline": 33, "eline": 33 }},
  "734932901": {"name": "X", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 4, "eline": 4 }, "descr": "x", {"code_orig": "1337", "file": "graph_deps.m", "sline": 36, "eline": 36 }},
  "763090144": {"name": "INPUT_UNDEFINED", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 15, "eline": 15 },  {"code_orig": "declared", "file": "graph_deps.m", "sline": 15, "eline": 15 }},
  "826352511": {"name": "A", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 11, "eline": 11 }, "descr": "a", {"code_orig": "1337", "file": "graph_deps.m", "sline": 34, "eline": 34 }},
  "848932841": {"name": "FLOAT", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 13, "eline": 13 }, "descr": "float", {"code_orig": "1337", "file": "graph_deps.m", "sline": 26, "eline": 26 }},
  "871028892": {"name": "Y", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 5, "eline": 5 }, "descr": "y", {"code_orig": "1337", "file": "graph_deps.m", "sline": 28, "eline": 28 }},
  "918369058": {"name": "CONST", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 12, "eline": 12 },  {"code_orig": "const", "file": "graph_deps.m", "sline": 12, "eline": 12 }},
  "991371585": {"name": "TAB", "is_input": false, "decl": {"code_orig": "declared", "file": "graph_deps.m", "sline": 18, "eline": 18 },  {"code_orig": "declared", "file": "graph_deps.m", "sline": 18, "eline": 18 }}},
  "runtimes": {
  
  "0": {"value": "42", "hash": 2813454 , "name" : "INPUT_DEFINED"},
  "1": {"value": "93", "hash": 661276726 , "name" : "INPUT_Y"},
  "2": {"value": "2022", "hash": 71606114 , "name" : "V_ANCSDED"},
  "3": {"value": "0", "hash": 95966794 , "name" : "X"},
  "4": {"value": "indefini", "hash": 991371585 , "name" : "TAB"},
  "5": {"value": "indefini", "hash": 82991271 , "name" : "VARTMP"},
  "6": {"value": "indefini", "hash": 991371585 , "name" : "TAB"},
  "7": {"value": "indefini", "hash": 141781586 , "name" : "TAB[0]"},
  "8": {"value": "indefini", "hash": 763090144 , "name" : "INPUT_UNDEFINED"},
  "9": {"value": "indefini", "hash": 185112339 , "name" : "Z"},
  "10": {"value": "0.123", "hash": 848932841 , "name" : "FLOAT"},
  "11": {"value": "3", "hash": 328171158 , "name" : "X"},
  "12": {"value": "6", "hash": 918369058 , "name" : "CONST"},
  "13": {"value": "114", "hash": 871028892 , "name" : "Y"},
  "14": {"value": "108", "hash": 259935139 , "name" : "TXMARJ"},
  "15": {"value": "150", "hash": 59860492 , "name" : "ANNEE"},
  "16": {"value": "117", "hash": 319850048 , "name" : "MULTILINE"},
  "17": {"value": "1", "hash": 731131379 , "name" : "VARTMP"},
  "18": {"value": "1", "hash": 826352511 , "name" : "A"},
  "19": {"value": "0", "hash": 478370311 , "name" : "I"},
  "20": {"value": "3", "hash": 734932901 , "name" : "X"},
  "21": {"value": "1", "hash": 478370311 , "name" : "I"},
  "22": {"value": "4", "hash": 734932901 , "name" : "X"},
  "23": {"value": "2", "hash": 478370311 , "name" : "I"},
  "24": {"value": "6", "hash": 734932901 , "name" : "X"},
  "25": {"value": "3", "hash": 478370311 , "name" : "I"},
  "26": {"value": "9", "hash": 734932901 , "name" : "X"},
  "27": {"value": "4", "hash": 478370311 , "name" : "I"},
  "28": {"value": "13", "hash": 734932901 , "name" : "X"},
  "29": {"value": "5", "hash": 478370311 , "name" : "I"},
  "30": {"value": "18", "hash": 734932901 , "name" : "X"},
  "31": {"value": "6", "hash": 478370311 , "name" : "I"},
  "32": {"value": "24", "hash": 734932901 , "name" : "X"},
  "33": {"value": "7", "hash": 478370311 , "name" : "I"},
  "34": {"value": "31", "hash": 734932901 , "name" : "X"},
  "35": {"value": "8", "hash": 478370311 , "name" : "I"},
  "36": {"value": "39", "hash": 734932901 , "name" : "X"},
  "37": {"value": "9", "hash": 478370311 , "name" : "I"},
  "38": {"value": "48", "hash": 734932901 , "name" : "X"},
  "39": {"value": "10", "hash": 478370311 , "name" : "I"},
  "40": {"value": "58", "hash": 734932901 , "name" : "X"},
  "12": {"name": "CONST", "value": "6", "kind": "const", "origin": {"code_orig": "const", "file": "graph_deps.m", "sline": 12, "eline": 12 }}},
  "interp_errors": {
  "40": {"name": "X", "value": 58, "expected": 3}},
  "aliases": {
  "INPUT_DEFINED": "IDEF",
  "INPUT_UNDEFINED": "IUND",
  "INPUT_Y": "IY",
  "BLABLA": "V_BLA",
  "V_ANCSDED": "V_POUET"
  }}

Calcul
  $ mlang calcul.m --mpp_function target -A app --run_test calcul.irj --no_nondet_display --trace --trace_output_file output.json
  [RESULT] calcul.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!
  $ cat output.json
  {"kind": "matrix", "graph":{
   "6": {"outcoming": [4,5]
  , "incoming": [7]
  },"3": {"outcoming": []
  , "incoming": [5]
  },"0": {"outcoming": []
  , "incoming": [7]
  },"5": {"outcoming": [3,4]
  , "incoming": [6]
  },"7": {"outcoming": [0,4,6]
  , "incoming": []
  },"4": {"outcoming": []
  , "incoming": [5,6,7]
  }},
  "statics": {
  
  "148128819": {"name": "TAUX", "is_input": false, "decl": {"code_orig": "declared", "file": "calcul.m", "sline": 8, "eline": 8 },  {"code_orig": "const", "file": "calcul.m", "sline": 8, "eline": 8 }},
  "163146027": {"name": "Y", "is_input": false, "decl": {"code_orig": "declared", "file": "calcul.m", "sline": 5, "eline": 5 }, "descr": "y", {"code_orig": "1337", "file": "calcul.m", "sline": 16, "eline": 16 }},
  "402052212": {"name": "TXMARJ", "is_input": false, "decl": {"code_orig": "declared", "file": "calcul.m", "sline": 6, "eline": 6 }, "descr": "TXMARJ", {"code_orig": "1337", "file": "calcul.m", "sline": 17, "eline": 17 }},
  "696860271": {"name": "X", "is_input": false, "decl": {"code_orig": "declared", "file": "calcul.m", "sline": 4, "eline": 4 }, "descr": "x", {"code_orig": "1337", "file": "calcul.m", "sline": 15, "eline": 15 }},
  "718198356": {"name": "V_ANCSDED", "is_input": true, "decl": {"code_orig": "input", "file": "calcul.m", "sline": 3, "eline": 3 }, "descr": "v_ancsed", {"code_orig": "input", "file": "calcul.m", "sline": 3, "eline": 3 }},
  "834005171": {"name": "X", "is_input": true, "decl": {"code_orig": "input", "file": "calcul.m", "sline": 4, "eline": 4 }, "descr": "x", {"code_orig": "input", "file": "calcul.m", "sline": 4, "eline": 4 }},
  "928325673": {"name": "REVENU", "is_input": false, "decl": {"code_orig": "declared", "file": "calcul.m", "sline": 9, "eline": 9 }, "descr": "revenu en fin", {"code_orig": "1337", "file": "calcul.m", "sline": 18, "eline": 18 }},
  "1047808456": {"name": "ENTREE", "is_input": true, "decl": {"code_orig": "input", "file": "calcul.m", "sline": 10, "eline": 10 }, "descr": "entree suite IRDV", {"code_orig": "input", "file": "calcul.m", "sline": 10, "eline": 10 }}},
  "runtimes": {
  
  "0": {"value": "24000", "hash": 1047808456 , "name" : "ENTREE"},
  "1": {"value": "2022", "hash": 718198356 , "name" : "V_ANCSDED"},
  "2": {"value": "0", "hash": 834005171 , "name" : "X"},
  "3": {"value": "3", "hash": 696860271 , "name" : "X"},
  "4": {"value": "20", "hash": 148128819 , "name" : "TAUX"},
  "5": {"value": "63", "hash": 163146027 , "name" : "Y"},
  "6": {"value": "43", "hash": 402052212 , "name" : "TXMARJ"},
  "7": {"value": "4843", "hash": 928325673 , "name" : "REVENU"},
  "4": {"name": "TAUX", "value": "20", "kind": "const", "origin": {"code_orig": "const", "file": "calcul.m", "sline": 8, "eline": 8 }}},
  "interp_errors": {
  },
  "aliases": {
  "ENTREE": "IDEF",
  "V_ANCSDED": "V_POUET"
  }}

Erreur
  $ mlang erreur.m --mpp_function target -A app --run_test erreur.irj --no_nondet_display --trace --trace_output_file output.json
  whatever
  [RESULT] erreur.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!
  $ cat output.json
  {"kind": "matrix", "graph":{
   },
  "statics": {
  
  "26797977": {"name": "X", "is_input": false, "decl": {"code_orig": "declared", "file": "erreur.m", "sline": 5, "eline": 5 }, "descr": "x", {"code_orig": "1", "file": "erreur.m", "sline": 9, "eline": 9 }},
  "57453478": {"name": "V_ANCSDED", "is_input": true, "decl": {"code_orig": "input", "file": "erreur.m", "sline": 4, "eline": 4 }, "descr": "v_ancsed", {"code_orig": "input", "file": "erreur.m", "sline": 4, "eline": 4 }},
  "168659796": {"name": "X", "is_input": true, "decl": {"code_orig": "input", "file": "erreur.m", "sline": 5, "eline": 5 }, "descr": "x", {"code_orig": "input", "file": "erreur.m", "sline": 5, "eline": 5 }}},
  "runtimes": {
  
  "0": {"value": "2022", "hash": 57453478 , "name" : "V_ANCSDED"},
  "1": {"value": "0", "hash": 168659796 , "name" : "X"},
  "2": {"value": "3", "hash": 26797977 , "name" : "X"}},
  "interp_errors": {
  },
  "aliases": {
  "V_ANCSDED": "V_ANCSDED"
  }}

M_EXT
  $ mlang m_ext.m --mpp_function test_args -A test --run_test m_ext.irj --no_nondet_display --trace --trace_output_file stderr
  entree test_args
    toto_cible(...) = 7
    toto_fonction(...) = 
  sortie test_args
  {"kind": "matrix", "graph":{
   },
  "statics": {
  
  "21870457": {"name": "V_ANCSDED", "is_input": true, "decl": {"code_orig": "input", "file": "m_ext.m", "sline": 3, "eline": 3 }, "descr": "v_ancsed", {"code_orig": "input", "file": "m_ext.m", "sline": 3, "eline": 3 }},
  "127330538": {"name": "A1", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 30, "eline": 30 }},
  "169899140": {"name": "A0", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 29, "eline": 29 }},
  "225517427": {"name": "A1", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 26, "eline": 26 }},
  "242643763": {"name": "A4", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 33, "eline": 33 }},
  "272047023": {"name": "A2", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 31, "eline": 31 }},
  "282347450": {"name": "I", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 28, "eline": 28 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 28, "eline": 28 }},
  "292826442": {"name": "R", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 26, "eline": 26 }},
  "313551748": {"name": "R", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 37, "eline": 37 }},
  "329215821": {"name": "A6", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 26, "eline": 26 }},
  "392990320": {"name": "A3", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 32, "eline": 32 }},
  "455051330": {"name": "A4", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 26, "eline": 26 }},
  "592757986": {"name": "A5", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 34, "eline": 34 }},
  "622913182": {"name": "A6", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 35, "eline": 35 }},
  "745964210": {"name": "A0", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 26, "eline": 26 }},
  "947675981": {"name": "A2", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 26, "eline": 26 }},
  "1018937179": {"name": "A3", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 26, "eline": 26 }},
  "1064150302": {"name": "A5", "is_input": false, "decl": {"code_orig": "declared", "file": "m_ext.m", "sline": 26, "eline": 26 },  {"code_orig": "target-test_args", "file": "m_ext.m", "sline": 26, "eline": 26 }}},
  "runtimes": {
  
  "0": {"value": "2026", "hash": 21870457 , "name" : "V_ANCSDED"},
  "1": {"value": "0", "hash": 282347450 , "name" : "I"},
  "2": {"value": "0", "hash": 745964210 , "name" : "A0"},
  "3": {"value": "0", "hash": 169899140 , "name" : "A0"},
  "4": {"value": "1", "hash": 225517427 , "name" : "A1"},
  "5": {"value": "1", "hash": 127330538 , "name" : "A1"},
  "6": {"value": "2", "hash": 947675981 , "name" : "A2"},
  "7": {"value": "2", "hash": 272047023 , "name" : "A2"},
  "8": {"value": "3", "hash": 1018937179 , "name" : "A3"},
  "9": {"value": "3", "hash": 392990320 , "name" : "A3"},
  "10": {"value": "4", "hash": 455051330 , "name" : "A4"},
  "11": {"value": "4", "hash": 242643763 , "name" : "A4"},
  "12": {"value": "5", "hash": 1064150302 , "name" : "A5"},
  "13": {"value": "5", "hash": 592757986 , "name" : "A5"},
  "14": {"value": "6", "hash": 329215821 , "name" : "A6"},
  "15": {"value": "6", "hash": 622913182 , "name" : "A6"},
  "16": {"value": "1", "hash": 282347450 , "name" : "I"},
  "17": {"value": "0", "hash": 745964210 , "name" : "A0"},
  "18": {"value": "0", "hash": 169899140 , "name" : "A0"},
  "19": {"value": "1", "hash": 225517427 , "name" : "A1"},
  "20": {"value": "1", "hash": 127330538 , "name" : "A1"},
  "21": {"value": "2", "hash": 947675981 , "name" : "A2"},
  "22": {"value": "2", "hash": 272047023 , "name" : "A2"},
  "23": {"value": "3", "hash": 1018937179 , "name" : "A3"},
  "24": {"value": "3", "hash": 392990320 , "name" : "A3"},
  "25": {"value": "4", "hash": 455051330 , "name" : "A4"},
  "26": {"value": "4", "hash": 242643763 , "name" : "A4"},
  "27": {"value": "5", "hash": 1064150302 , "name" : "A5"},
  "28": {"value": "5", "hash": 592757986 , "name" : "A5"},
  "29": {"value": "6", "hash": 329215821 , "name" : "A6"},
  "30": {"value": "6", "hash": 622913182 , "name" : "A6"},
  "31": {"value": "2", "hash": 282347450 , "name" : "I"},
  "32": {"value": "0", "hash": 745964210 , "name" : "A0"},
  "33": {"value": "0", "hash": 169899140 , "name" : "A0"},
  "34": {"value": "1", "hash": 225517427 , "name" : "A1"},
  "35": {"value": "1", "hash": 127330538 , "name" : "A1"},
  "36": {"value": "2", "hash": 947675981 , "name" : "A2"},
  "37": {"value": "2", "hash": 272047023 , "name" : "A2"},
  "38": {"value": "3", "hash": 1018937179 , "name" : "A3"},
  "39": {"value": "3", "hash": 392990320 , "name" : "A3"},
  "40": {"value": "4", "hash": 455051330 , "name" : "A4"},
  "41": {"value": "4", "hash": 242643763 , "name" : "A4"},
  "42": {"value": "5", "hash": 1064150302 , "name" : "A5"},
  "43": {"value": "5", "hash": 592757986 , "name" : "A5"},
  "44": {"value": "6", "hash": 329215821 , "name" : "A6"},
  "45": {"value": "6", "hash": 622913182 , "name" : "A6"},
  "46": {"value": "3", "hash": 282347450 , "name" : "I"},
  "47": {"value": "0", "hash": 745964210 , "name" : "A0"},
  "48": {"value": "0", "hash": 169899140 , "name" : "A0"},
  "49": {"value": "1", "hash": 225517427 , "name" : "A1"},
  "50": {"value": "1", "hash": 127330538 , "name" : "A1"},
  "51": {"value": "2", "hash": 947675981 , "name" : "A2"},
  "52": {"value": "2", "hash": 272047023 , "name" : "A2"},
  "53": {"value": "3", "hash": 1018937179 , "name" : "A3"},
  "54": {"value": "3", "hash": 392990320 , "name" : "A3"},
  "55": {"value": "4", "hash": 455051330 , "name" : "A4"},
  "56": {"value": "4", "hash": 242643763 , "name" : "A4"},
  "57": {"value": "5", "hash": 1064150302 , "name" : "A5"},
  "58": {"value": "5", "hash": 592757986 , "name" : "A5"},
  "59": {"value": "6", "hash": 329215821 , "name" : "A6"},
  "60": {"value": "6", "hash": 622913182 , "name" : "A6"},
  "61": {"value": "4", "hash": 282347450 , "name" : "I"},
  "62": {"value": "0", "hash": 745964210 , "name" : "A0"},
  "63": {"value": "0", "hash": 169899140 , "name" : "A0"},
  "64": {"value": "1", "hash": 225517427 , "name" : "A1"},
  "65": {"value": "1", "hash": 127330538 , "name" : "A1"},
  "66": {"value": "2", "hash": 947675981 , "name" : "A2"},
  "67": {"value": "2", "hash": 272047023 , "name" : "A2"},
  "68": {"value": "3", "hash": 1018937179 , "name" : "A3"},
  "69": {"value": "3", "hash": 392990320 , "name" : "A3"},
  "70": {"value": "4", "hash": 455051330 , "name" : "A4"},
  "71": {"value": "4", "hash": 242643763 , "name" : "A4"},
  "72": {"value": "5", "hash": 1064150302 , "name" : "A5"},
  "73": {"value": "5", "hash": 592757986 , "name" : "A5"},
  "74": {"value": "6", "hash": 329215821 , "name" : "A6"},
  "75": {"value": "6", "hash": 622913182 , "name" : "A6"},
  "76": {"value": "5", "hash": 282347450 , "name" : "I"},
  "77": {"value": "0", "hash": 745964210 , "name" : "A0"},
  "78": {"value": "0", "hash": 169899140 , "name" : "A0"},
  "79": {"value": "1", "hash": 225517427 , "name" : "A1"},
  "80": {"value": "1", "hash": 127330538 , "name" : "A1"},
  "81": {"value": "2", "hash": 947675981 , "name" : "A2"},
  "82": {"value": "2", "hash": 272047023 , "name" : "A2"},
  "83": {"value": "3", "hash": 1018937179 , "name" : "A3"},
  "84": {"value": "3", "hash": 392990320 , "name" : "A3"},
  "85": {"value": "4", "hash": 455051330 , "name" : "A4"},
  "86": {"value": "4", "hash": 242643763 , "name" : "A4"},
  "87": {"value": "5", "hash": 1064150302 , "name" : "A5"},
  "88": {"value": "5", "hash": 592757986 , "name" : "A5"},
  "89": {"value": "6", "hash": 329215821 , "name" : "A6"},
  "90": {"value": "6", "hash": 622913182 , "name" : "A6"},
  "91": {"value": "6", "hash": 282347450 , "name" : "I"},
  "92": {"value": "0", "hash": 745964210 , "name" : "A0"},
  "93": {"value": "0", "hash": 169899140 , "name" : "A0"},
  "94": {"value": "1", "hash": 225517427 , "name" : "A1"},
  "95": {"value": "1", "hash": 127330538 , "name" : "A1"},
  "96": {"value": "2", "hash": 947675981 , "name" : "A2"},
  "97": {"value": "2", "hash": 272047023 , "name" : "A2"},
  "98": {"value": "3", "hash": 1018937179 , "name" : "A3"},
  "99": {"value": "3", "hash": 392990320 , "name" : "A3"},
  "100": {"value": "4", "hash": 455051330 , "name" : "A4"},
  "101": {"value": "4", "hash": 242643763 , "name" : "A4"},
  "102": {"value": "5", "hash": 1064150302 , "name" : "A5"},
  "103": {"value": "5", "hash": 592757986 , "name" : "A5"},
  "104": {"value": "6", "hash": 329215821 , "name" : "A6"},
  "105": {"value": "6", "hash": 622913182 , "name" : "A6"},
  "106": {"value": "7", "hash": 292826442 , "name" : "R"},
  "107": {"value": "7", "hash": 313551748 , "name" : "R"}},
  "interp_errors": {
  },
  "aliases": {
  "V_ANCSDED": "V_POUET"
  }}
  [RESULT] m_ext.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!

Wrong
  $ mlang wrong.m --mpp_function target -A app --run_test wrong.irj --no_nondet_display --trace
  {"kind": "matrix", "graph":{
   "1": {"outcoming": []
  , "incoming": [2]
  },"2": {"outcoming": [1]
  , "incoming": []
  }},
  "statics": {
  
  "192282778": {"name": "Y", "is_input": false, "decl": {"code_orig": "declared", "file": "wrong.m", "sline": 5, "eline": 5 }, "descr": "y", {"code_orig": "1337", "file": "wrong.m", "sline": 9, "eline": 9 }},
  "598841962": {"name": "V_ANCSDED", "is_input": true, "decl": {"code_orig": "input", "file": "wrong.m", "sline": 3, "eline": 3 }, "descr": "v_ancsed", {"code_orig": "input", "file": "wrong.m", "sline": 3, "eline": 3 }},
  "741196454": {"name": "X", "is_input": false, "decl": {"code_orig": "declared", "file": "wrong.m", "sline": 4, "eline": 4 }, "descr": "x", {"code_orig": "1337", "file": "wrong.m", "sline": 10, "eline": 10 }}},
  "runtimes": {
  
  "0": {"value": "2022", "hash": 598841962 , "name" : "V_ANCSDED"},
  "1": {"value": "3", "hash": 192282778 , "name" : "Y"},
  "2": {"value": "3", "hash": 741196454 , "name" : "X"}},
  "interp_errors": {
  },
  "aliases": {
  "V_ANCSDED": "V_POUET"
  }}
  [RESULT] wrong.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!

Tab
  $ mlang tab.m --mpp_function target -A app --run_test tab.irj --no_nondet_display --trace --trace_output_file output.json
  [WARNING] Variable "TAB" définie plus d'une fois dans la même règle
  
    --> tab.m
     | 
  11 | TAB[1] = 3;
     | ^^^^^^
  
    --> tab.m
     | 
  12 | TAB[0] = Y;
     | ^^^^^^
  
    --> tab.m
     | 
  13 | TAB[2] = TAB[1];
     | ^^^^^^
  
  [WARNING] Variable "Z" définie plus d'une fois dans la même règle
  
    --> tab.m
     | 
  14 | Z = TAB[3];
     | ^
  
    --> tab.m
     | 
  16 | Z = 123;
     | ^
  
  [RESULT] tab.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!
  $ cat output.json
  {"kind": "matrix", "graph":{
   "1": {"outcoming": []
  , "incoming": [3]
  },"6": {"outcoming": [5]
  , "incoming": []
  },"3": {"outcoming": [1]
  , "incoming": [7]
  },"5": {"outcoming": []
  , "incoming": [6]
  },"7": {"outcoming": [3]
  , "incoming": []
  },"4": {"outcoming": [2]
  , "incoming": []
  },"2": {"outcoming": []
  , "incoming": [4]
  }},
  "statics": {
  
  "46735252": {"name": "V_ANCSDED", "is_input": true, "decl": {"code_orig": "input", "file": "tab.m", "sline": 3, "eline": 3 }, "descr": "v_ancsed", {"code_orig": "input", "file": "tab.m", "sline": 3, "eline": 3 }},
  "350853964": {"name": "X", "is_input": false, "decl": {"code_orig": "declared", "file": "tab.m", "sline": 4, "eline": 4 }, "descr": "x", {"code_orig": "1337", "file": "tab.m", "sline": 15, "eline": 15 }},
  "387035677": {"name": "Z", "is_input": false, "decl": {"code_orig": "declared", "file": "tab.m", "sline": 6, "eline": 6 }, "descr": "z", {"code_orig": "1337", "file": "tab.m", "sline": 16, "eline": 16 }},
  "458794046": {"name": "Z", "is_input": false, "decl": {"code_orig": "declared", "file": "tab.m", "sline": 6, "eline": 6 }, "descr": "z", {"code_orig": "1337", "file": "tab.m", "sline": 14, "eline": 14 }},
  "571677703": {"name": "TAB[2]", "is_input": false, "decl": {"code_orig": "declared", "file": "tab.m", "sline": 7, "eline": 7 }, "descr": "tableau", {"code_orig": "1337", "file": "tab.m", "sline": 13, "eline": 13 }},
  "576561133": {"name": "TAB[0]", "is_input": false, "decl": {"code_orig": "declared", "file": "tab.m", "sline": 7, "eline": 7 }, "descr": "tableau", {"code_orig": "1337", "file": "tab.m", "sline": 12, "eline": 12 }},
  "799751457": {"name": "Y", "is_input": true, "decl": {"code_orig": "input", "file": "tab.m", "sline": 5, "eline": 5 }, "descr": "y", {"code_orig": "input", "file": "tab.m", "sline": 5, "eline": 5 }},
  "961470799": {"name": "TAB[1]", "is_input": false, "decl": {"code_orig": "declared", "file": "tab.m", "sline": 7, "eline": 7 }, "descr": "tableau", {"code_orig": "1337", "file": "tab.m", "sline": 11, "eline": 11 }},
  "1064319348": {"name": "TAB", "is_input": false, "decl": {"code_orig": "declared", "file": "tab.m", "sline": 7, "eline": 7 },  {"code_orig": "declared", "file": "tab.m", "sline": 7, "eline": 7 }}},
  "runtimes": {
  
  "0": {"value": "2022", "hash": 46735252 , "name" : "V_ANCSDED"},
  "1": {"value": "12", "hash": 799751457 , "name" : "Y"},
  "2": {"value": "3", "hash": 961470799 , "name" : "TAB[1]"},
  "3": {"value": "12", "hash": 576561133 , "name" : "TAB[0]"},
  "4": {"value": "3", "hash": 571677703 , "name" : "TAB[2]"},
  "5": {"value": "indefini", "hash": 1064319348 , "name" : "TAB"},
  "6": {"value": "indefini", "hash": 458794046 , "name" : "Z"},
  "7": {"value": "12", "hash": 350853964 , "name" : "X"},
  "8": {"value": "123", "hash": 387035677 , "name" : "Z"}},
  "interp_errors": {
  },
  "aliases": {
  "V_ANCSDED": "V_POUET"
  }}
