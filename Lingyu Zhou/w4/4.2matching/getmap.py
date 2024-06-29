import pandas as pd
import json

map=pd.read_excel("audit_var_codebook_simple.xlsx")[["var name","potential key terms"]]
map["potential key terms"]=map["potential key terms"].str.replace('"','').str.replace(";","").str.strip()
mapd=dict(zip(map["var name"],map["potential key terms"].str.split(" or ")))
for k in mapd:
    mapd[k]=[x.lower().strip() for x in mapd[k]]

with open("map.json", "w") as m:
    m.writelines(json.dumps(mapd))
