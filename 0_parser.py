from lxml import etree as ET
import os
import pandas as PD
from datetime import datetime
path = 'E:/Research/Data_pubmed/baseline23'
save = 'E:/Research/Data_pubmed/processed_data6/'


for filename in os.listdir(path)[:]:
    print(datetime.now().strftime("%d/%m/%Y %H:%M:%S") + " Started working with: " + filename)
    if not filename.endswith('.xml'): continue
    fullname = os.path.join(path, filename)
    tree = ET.parse(fullname)
    root = tree.getroot()
    out_data = PD.DataFrame()
    pub_counter = 0
    
    for pub in root:
            pub_counter += 1
            ISSN_j = "NA"
            ISSN_l = "NA"
            try:
                ISSN_j = pub.find("MedlineCitation/Article/Journal/ISSN").text
            except:
                pass
            try:
                ISSN_l = pub.find("MedlineCitation/MedlineJournalInfo/ISSNLinking").text
            except:
                pass
            temp_record = {
                'pmid': int(pub.find('PubmedData/ArticleIdList/ArticleId[@IdType="pubmed"]').text), #PMID
                'year': pub.find("PubmedData/History/PubMedPubDate/Year").text, #Year
                'ISSN_journal': ISSN_j,
                'ISSN_linking': ISSN_l,
                'month': pub.find("PubmedData/History/PubMedPubDate/Month").text, # month
                'mesh_descriptor': str([mesh.text for mesh in pub.findall('MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName[@MajorTopicYN="N"]')]), # mesh
                'mesh_descriptor_major': str([mesh.text for mesh in pub.findall('MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName[@MajorTopicYN="Y"]')]), # mesh
                'mesh_qualifier': str([mesh.text for mesh in pub.findall('MedlineCitation/MeshHeadingList/MeshHeading/QualifierName[@MajorTopicYN="N"]')]), # mesh
                'mesh_qualifier_major': str([mesh.text for mesh in pub.findall('MedlineCitation/MeshHeadingList/MeshHeading/QualifierName[@MajorTopicYN="Y"]')]), # mesh
                'mesh_descriptor_UI': str([mesh.attrib['UI'] for mesh in pub.findall('MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName[@MajorTopicYN="N"]')]), # mesh,
                'mesh_qualifier_UI': str([mesh.attrib['UI'] for mesh in pub.findall('MedlineCitation/MeshHeadingList/MeshHeading/QualifierName[@MajorTopicYN="N"]')]), # mesh,
                'mesh_major_descriptor_UI': str([mesh.attrib['UI'] for mesh in pub.findall('MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName[@MajorTopicYN="Y"]')]), # mesh,
                'mesh_major_qualifier_UI': str([mesh.attrib['UI'] for mesh in pub.findall('MedlineCitation/MeshHeadingList/MeshHeading/QualifierName[@MajorTopicYN="Y"]')]), # mesh,
                'mesh_descriptor_for_major_qualifier': str([mesh.attrib['UI'] for mesh in pub.findall('MedlineCitation/MeshHeadingList/MeshHeading/QualifierName[@MajorTopicYN="Y"]/../DescriptorName')]),
                'keywords': str([mesh.text for mesh in pub.findall('MedlineCitation/KeywordList/Keyword')]), # keywords
                'refs': str([ref.text for ref in pub.findall('PubmedData/ReferenceList/Reference/ArticleIdList/ArticleId[@IdType="pubmed"]')]) # pubmed refs
            }
            tempdf = PD.DataFrame(temp_record, index=[0])
            out_data = out_data.append(tempdf, ignore_index =True)
            # Timestamp every 100 records
            if pub_counter % 500 == 0:
                print(datetime.now().strftime("%d/%m/%Y %H:%M:%S") + " Processed records: " + str(pub_counter))
  
    out_data.to_csv(save + 'out_' + filename[:-4] + '_' + str(pub_counter) + '.csv', index=False)
    print(datetime.now().strftime("%d/%m/%Y %H:%M:%S") + " Data saved to: " + save + 'out_' + filename[:-4] + '_' + str(pub_counter) + '.csv')