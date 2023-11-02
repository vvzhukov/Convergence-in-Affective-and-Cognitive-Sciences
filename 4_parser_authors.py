from lxml import etree as ET
import os
import pandas as PD
from datetime import datetime
path = 'E:/Research/Data_pubmed/baseline23'
save = 'E:/Research/Data_pubmed/processed_data_authors2/'


for filename in os.listdir(path)[1118:]:
    print(datetime.now().strftime("%d/%m/%Y %H:%M:%S") + " Started working with: " + filename)
    if not filename.endswith('.xml'): continue
    fullname = os.path.join(path, filename)
    tree = ET.parse(fullname)
    root = tree.getroot()
    out_data = PD.DataFrame()
    pub_counter = 0
    
    for pub in root:
            pub_counter += 1
            pmid = int(pub.find('PubmedData/ArticleIdList/ArticleId[@IdType="pubmed"]').text), #PMID
            
            for auth in pub.findall('MedlineCitation/Article/AuthorList/Author'):
                
                lastname = "NA"
                forename = "NA"
                initials =  "NA"
                suffix =  "NA"
                collectivename =  "NA"
                identifier =  "NA"
                affiliationinfo = "NA"
                
                try:
                    lastname = auth.find('LastName').text
                except:
                    pass
                try:
                    forename = auth.find('ForeName').text
                except:
                    pass
                try:
                    initials = auth.find('Initials').text
                except:
                    pass
                try:
                    suffix = auth.find('Suffix').text
                except:
                    pass
                try:
                    collectivename = auth.find('CollectiveName').text
                except:
                    pass
                try:
                    identifier = auth.find('Identifier').text
                except:
                    pass
                try:
                    affiliationinfo = auth.find('AffiliationInfo/Affiliation').text
                except:
                    pass
                
                temp_record = {
                    'pmid': pmid,
                    'lastname': lastname,
                    'forename': forename,
                    'initials': initials,
                    'suffix': suffix,
                    'collectivename': collectivename,
                    'identifier': identifier,
                    'affiliationinfo': affiliationinfo
                }
                tempdf = PD.DataFrame(temp_record, index=[0])
                out_data = out_data.append(tempdf, ignore_index =True)
                
            # Timestamp every 500 records
            if pub_counter % 500 == 0:
                print(datetime.now().strftime("%d/%m/%Y %H:%M:%S") + " Processed records: " + str(pub_counter))
    try:
        out_data.to_csv(save + 'out_' + filename[:-4] + '_' + str(pub_counter) + '.csv', index=False)
        print(datetime.now().strftime("%d/%m/%Y %H:%M:%S") + " Data saved to: " + save + 'out_' + filename[:-4] + '_' + str(pub_counter) + '.csv')
        
    except:
        print('Error saving file!')
    