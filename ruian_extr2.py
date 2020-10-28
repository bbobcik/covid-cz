#!python
import sys
import json
from lxml import etree
sys.stdout.reconfigure(encoding='utf-8')

Stat = {}
Kraj = {}
Okres = {}
Orp = {}
Pou = {}
Obec = {}

KrajById = {}
OkresById = {}
ObecById = {}

def format_pos(pos):
	return ';'.join(pos.text.split())
	
def format_ref_pos(pos_spec):
	return [ int(float(coord)) for coord in pos_spec.split() ]
	
def extract_pos_list(elem):
	pos_list = elem.find('.//{http://www.opengis.net/gml/3.2}posList').text
	return [ int(float(coord)) for coord in pos_list.split() ]
	
def extract_bounds(bounds_elem):
	surface = bounds_elem.find('./{http://www.opengis.net/gml/3.2}MultiSurface')
	exterior = surface.find('.//{http://www.opengis.net/gml/3.2}exterior')
	bounds = [ extract_pos_list(exterior) ]
	interior = surface.find('.//{http://www.opengis.net/gml/3.2}interior')
	if etree.iselement(interior):
		bounds.append(extract_pos_list(interior))
	return bounds

def process_stat(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:StatIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	kod = int(kod.text)
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:StatIntTypy:v1}Nazev').text
	nuts = elem.find('./{urn:cz:isvs:ruian:schemas:StatIntTypy:v1}NutsLau').text
	geom = elem.find('./{urn:cz:isvs:ruian:schemas:StatIntTypy:v1}Geometrie')
	pos = geom.find('./{urn:cz:isvs:ruian:schemas:StatIntTypy:v1}DefinicniBod//{http://www.opengis.net/gml/3.2}pos').text
	bounds = geom.find('./{urn:cz:isvs:ruian:schemas:StatIntTypy:v1}GeneralizovaneHranice5')
	Stat[nuts] = {
		'id': kod,
		'name': nazev,
		'nuts': nuts,
		'ref_pos': format_ref_pos(pos),
		'boundary': extract_bounds(bounds),
	}

def process_kraj(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	kod = kod.text
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}Nazev').text
	nuts = elem.find('./{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}NutsLau').text
	geom = elem.find('./{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}Geometrie')
	pos = geom.find('./{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}DefinicniBod//{http://www.opengis.net/gml/3.2}pos').text
	bounds = geom.find('./{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}GeneralizovaneHranice5')
	Kraj[nuts] = {
		'id': int(kod),
		'name': nazev,
		'nuts': nuts,
		'ref_pos': format_ref_pos(pos),
		'boundary': extract_bounds(bounds),
	}
	KrajById[kod] = Kraj[nuts]

def process_okres(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	kod = kod.text
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}Nazev').text
	nuts = elem.find('./{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}NutsLau').text
	kraj = int(elem.find('.//{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}Kod').text)
	geom = elem.find('./{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}Geometrie')
	pos = geom.find('./{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}DefinicniBod//{http://www.opengis.net/gml/3.2}pos').text
	bounds = geom.find('./{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}GeneralizovaneHranice4')
	Okres[nuts] = {
		'id': int(kod),
		'name': nazev,
		'nuts': nuts,
		'region_id': kraj,
		'ref_pos': format_ref_pos(pos),
		'boundary': extract_bounds(bounds),
	}
	OkresById[kod] = Okres[nuts]

def process_orp(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	kod = kod.text
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}Nazev').text
	spravni_obec = int(elem.find('./{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}SpravniObecKod').text)
	kraj_id = elem.find('.//{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}Kod').text
	geom = elem.find('./{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}Geometrie')
	pos = geom.find('./{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}DefinicniBod//{http://www.opengis.net/gml/3.2}pos').text
	bounds = geom.find('./{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}GeneralizovaneHranice4')
	kraj = KrajById[kraj_id]
	Orp[kod] = {
		'id': int(kod),
		'name': nazev,
		'ref_place': spravni_obec,
		'region_nuts': kraj['nuts'],
		'region_name': kraj['name'],
		'ref_pos': format_ref_pos(pos),
		'boundary': extract_bounds(bounds),
	}

def process_pou(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	kod = kod.text
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}Nazev').text
	spravni_obec = int(elem.find('./{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}SpravniObecKod').text)
	orp_id = elem.find('.//{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}Kod').text
	geom = elem.find('./{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}Geometrie')
	pos = geom.find('./{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}DefinicniBod//{http://www.opengis.net/gml/3.2}pos').text
	bounds = geom.find('./{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}GeneralizovaneHranice4')
	orp = Orp[orp_id]
	Pou[kod] = {
		'id': int(kod),
		'name': nazev,
		'ref_place': spravni_obec,
		'region_nuts': orp['region_nuts'],
		'region_name': orp['region_name'],
		'orp_id': int(orp_id),
		'orp_name': orp['name'],
		'ref_pos': format_ref_pos(pos),
		'boundary': extract_bounds(bounds),
	}

def process_obec(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	kod = kod.text
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}Nazev').text
	nuts = elem.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}NutsLau').text
	status = elem.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}StatusKod').text
	okres_id = elem.find('.//{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}Kod').text
	pou_id = elem.find('.//{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}Kod').text
	geom = elem.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}Geometrie')
	pos = geom.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}DefinicniBod//{http://www.opengis.net/gml/3.2}pos').text
	bounds = geom.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}GeneralizovaneHranice3')
	pou = Pou[pou_id]
	okres = OkresById[okres_id]
	Obec[nuts] = {
		'id': int(kod),
		'name': nazev,
		'nuts': nuts,
		'status': int(status),
		'region_nuts': pou['region_nuts'],
		'region_name': pou['region_name'],
		'district_nuts': okres['nuts'],
		'district_name': okres['name'],
		'orp_id': pou['orp_id'],
		'orp_name': pou['orp_name'],
		'pou_id': int(pou_id),
		'pou_name': pou['name'],		
		'ref_pos': format_ref_pos(pos),
		'boundary': extract_bounds(bounds),
	}
	ObecById[kod] = Obec[nuts]

def dump_element(elem):
	print(etree.tostring(elem, encoding='unicode', pretty_print=True, with_tail=False))

for _, element in etree.iterparse('20200930_ST_UKSG.xml', huge_tree=True, remove_comments=True):
	processed = True
	if element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Stat':
		process_stat(element)
	elif element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Vusc':
		process_kraj(element)
#		dump_element(element)
#		break
	elif element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Okres':
		process_okres(element)
#		dump_element(element)
#		break
	elif element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Orp':
		process_orp(element)
#		dump_element(element)
#		break
	elif element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Pou':
		process_pou(element)
#		dump_element(element)
#		break
	elif element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Obec':
#		dump_element(element)
#		break
		process_obec(element)
	else:
		processed = False
	if processed:
		element.clear(keep_tail=True)


def dump_spec(data, filename):
	with open(filename, 'w') as f:
		json.dump(data, f, indent=4, separators=(',', ': '))

dump_spec(Stat, 'data/ruian/01-country.json')
dump_spec(Kraj, 'data/ruian/02-region.json')
dump_spec(Okres, 'data/ruian/03-district.json')
dump_spec(Orp, 'data/ruian/04-orp.json')
dump_spec(Pou, 'data/ruian/05-pou.json')
dump_spec(Obec, 'data/ruian/06-place.json')
