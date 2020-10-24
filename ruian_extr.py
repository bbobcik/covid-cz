#!python
import sys
from lxml import etree
sys.stdout.reconfigure(encoding='utf-8')

def format_pos(pos):
	return ';'.join(pos.text.split())

def process_kraj(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}Nazev')
	nuts = elem.find('./{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}NutsLau')
	pos = elem.find('.//{http://www.opengis.net/gml/3.2}pos')
	print(f'KRAJ;{kod.text};{nazev.text};{nuts.text};;;;;;;{format_pos(pos)}')

def process_okres(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}Nazev')
	nuts = elem.find('./{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}NutsLau')
	kraj = elem.find('.//{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}Kod')
	pos = elem.find('.//{http://www.opengis.net/gml/3.2}pos')
	print(f'OKRES;{kod.text};{nazev.text};{nuts.text};{kraj.text};;;;;;{format_pos(pos)}')

def process_orp(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}Nazev')
	spravni_obec = elem.find('./{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}SpravniObecKod')
	kraj = elem.find('.//{urn:cz:isvs:ruian:schemas:VuscIntTypy:v1}Kod')
	pos = elem.find('.//{http://www.opengis.net/gml/3.2}pos')
	print(f'ORP;{kod.text};{nazev.text};;{kraj.text};;;;{spravni_obec.text};;{format_pos(pos)}')

def process_pou(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}Nazev')
	spravni_obec = elem.find('./{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}SpravniObecKod')
	orp = elem.find('.//{urn:cz:isvs:ruian:schemas:OrpIntTypy:v1}Kod')
	pos = elem.find('.//{http://www.opengis.net/gml/3.2}pos')
	print(f'PUI;{kod.text};{nazev.text};;;;{orp.text};;{spravni_obec.text};;{format_pos(pos)}')

def process_obec(elem):
	kod = elem.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}Kod')
	if not etree.iselement(kod):
		return
	nazev = elem.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}Nazev')
	nuts = elem.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}NutsLau')
	status = elem.find('./{urn:cz:isvs:ruian:schemas:ObecIntTypy:v1}StatusKod')
	okres = elem.find('.//{urn:cz:isvs:ruian:schemas:OkresIntTypy:v1}Kod')
	pou = elem.find('.//{urn:cz:isvs:ruian:schemas:PouIntTypy:v1}Kod')
	pos = elem.find('.//{http://www.opengis.net/gml/3.2}pos')
	print(f'OBEC;{kod.text};{nazev.text};{nuts.text};;{okres.text};;{pou.text};;{status.text};{format_pos(pos)}')

def dump_element(elem):
	print(etree.tostring(elem, encoding='unicode', pretty_print=True, with_tail=False))

for _, element in etree.iterparse('20200930_ST_UZSZ.xml', huge_tree=True, remove_comments=True):
	processed = True
	if element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Vusc':
		process_kraj(element)
	elif element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Okres':
		process_okres(element)
	elif element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Orp':
		process_orp(element)
	elif element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Pou':
		process_pou(element)
	elif element.tag == '{urn:cz:isvs:ruian:schemas:VymennyFormatTypy:v1}Obec':
		process_obec(element)
#		dump_element(element)
#		break
	else:
		processed = False
	if processed:
		element.clear(keep_tail=True)
