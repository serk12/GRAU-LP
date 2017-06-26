#!/usr/bin/python3
import os
import sys
import argparse
import urllib.request
import xml.etree.ElementTree as ET
import collections
from math import radians, cos, sin, atan2, sqrt
from ast import literal_eval
import unicodedata

URL_BICING = "http://wservice.viabicing.cat/v1/getstations.php?v=1"
LENG_URBAN = {'cat': "http://www.bcn.cat/tercerlloc/pits_opendata.xml",
              'es': "http://www.bcn.cat/tercerlloc/pits_opendata_es.xml",
              'en': "http://www.bcn.cat/tercerlloc/pits_opendata_en.xml",
              'fr': "http://www.bcn.cat/tercerlloc/pits_opendata_fr.xml"}


def parseParam():
    parser = argparse.ArgumentParser(description='Get HELP OTHER PLACE.')
    parser.add_argument(
        '--lan', nargs=1, default=['cat'], choices=['cat', 'fr', 'en', 'es'])
    parser.add_argument('--key', nargs=1)
    return parser.parse_args()


def getXmlTree(url):
    try:
        fileXml = urllib.request.urlopen(url)
    except:
        print ("No conection to internet")
        sys.exit()
    xmlString = fileXml.read()
    fileXml.close()
    return ET.fromstring(xmlString)


class BicingStop:
    def __init__(self, pos, street, num, qttyBic, slots):
        self.pos = pos
        self.street = street
        self.num = num
        self.qttyBic = int(qttyBic)
        self.slots = int(slots)


def hasSpots(bike):
    return bike.slots > 0


def hasBikes(bike):
    return bike.qttyBic > 0


class Place:
    def __init__(self, district, street, barri, name, short, content, pos):
        self.district = district
        self.street = street
        self.barri = barri
        self.name = name
        self.short = short
        self.content = content
        self.pos = pos

    def getBikes(self, bikes, dist, method):
        bikesClose = collections.OrderedDict()
        for aux in bikes:
            distB = self.pos.distance(aux.pos)
            if distB < dist and method(aux):
                bikesClose[distB] = aux
        first5 = list(bikesClose.items())[:5]
        out = ""
        for i in range(0, min(5, len(first5))):
            _, bik = first5[i]
            out += bik.street + ", " + bik.num + "<br>"
        return out


class Position:
    RADI_TERRA = 6371000

    def __init__(self, lat, lon):
        self.latitud, self.longitud = map(radians, (float(lat), float(lon)))

    def distance(self, point):
        dlon = self.longitud - point.longitud
        dlat = self.latitud - point.latitud
        a = sin(dlat / 2)**2 + cos(point.longitud) * \
            cos(self.longitud) * sin(dlon / 2)**2
        c = 2 * atan2(sqrt(a), sqrt(1 - a))
        return self.RADI_TERRA * c


def getBikes():
    xmlTree = getXmlTree(URL_BICING)
    STATUS = "OPN"
    COLUMNS = ("lat", "long", "street", "streetNumber",
               "bikes", "slots", "status")
    if xmlTree is None:
        return []
    stopsBicing = []
    for i in range(1, len(xmlTree)):
        station = xmlTree[i]
        lat, lon, street, num, qttyBic, slots, estat = map(
            lambda x: station.findtext(x), COLUMNS)
        if estat == STATUS and (int(qttyBic) > 0 or int(slots) > 0):
            stopsBicing.append(BicingStop(
                Position(lat, lon), street, num, qttyBic, slots))
    return stopsBicing


def adapt(input_str):
    nfkd_form = unicodedata.normalize('NFKD', input_str)
    aux = u"".join([c for c in nfkd_form if not unicodedata.combining(c)])
    return aux.lower()


def evalSwitch(key, param, place):
    item = adapt(param)
    if key == "default":
        return item in adapt(place.name) or item in adapt(place.street) or\
            item in adapt(place.district) or item in adapt(place.barri)
    elif key == "name":
        return item in adapt(place.name)
    elif key == "content":
        return item in adapt(place.content)
    elif key == "location":
        return item in adapt(place.district) or item in adapt(place.street)\
            or item in adapt(place.barri)


def evalDict(key, value, place):
    if (type(value) is tuple or type(value) is list):
        result = evalDict(key, value[0], place)
        for item in value[1:]:
            if (key is tuple):
                result = result and evalSwitch(key, item, place)
            else:
                result = result or evalSwitch(key, item, place)
    else:
        result = evalSwitch(key, value, place)
    return result


def evalArguments(argument, place):
    key = type(argument)
    if (key is tuple or key is list):
        result = evalArguments(argument[0], place)
        for item in argument[1:]:
            if (key is tuple):
                result = result and evalArguments(item, place)
            else:
                result = result or evalArguments(item, place)
    elif (key is dict):
        result = True
        for pas, value in argument.items():
            result = result and evalDict(pas, value, place)
    else:
        result = evalSwitch("default", argument, place)
    return result


def getPlaces(lan, key):
    xmlTree = getXmlTree(LENG_URBAN[lan[0]])
    TAGS = ("addresses/item/district", "addresses/item/address",
            "addresses/item/barri", "custom_fields/descripcio-curta-pics",
            "name", "content", "gmapx", "gmapy")
    if xmlTree is None or xmlTree[1] is None:
        return []
    places = []
    if (key):
        if (len(key) > 0):
            key = literal_eval(key[0])
    for i in range(1, len(xmlTree[1])):
        place = xmlTree[1][i]
        district, address, barri, short, name, content, gmapx, gmapy = map(
            lambda x: place.findtext(x),  TAGS)
        place = Place(district, address, barri, name, short,
                      content, Position(gmapx, gmapy))
        try:
            if not key or len(key) <= 0 or evalArguments(key, place):
                places.append(place)
        except:
            continue
    return places


def generateHTML():
    idsBici = set({})
    bodyPlaces = ""
    headerPlaces = "<tr><th>name</th><th>content</th><th>Adreça</th><th>Dir.\
    Bicing disponible (parking)</th><th>Dir. Bicing disponible</th></tr>"
    moreThanTwo = len(places) > 1
    for place in places:
        auxIdsSlot = place.getBikes(bicings, 500, hasSpots)
        auxIdsBici = place.getBikes(bicings, 500, hasBikes)
        try:
            textDesc = place.short if moreThanTwo else place.content
            bodyPlaces += "<tr><td>" + place.name + "</td><td>" + \
                textDesc + "</td><td>" + \
                place.street + "</td><td>" + auxIdsSlot + \
                "</td><td>" + auxIdsBici + "</td></tr>"
        except:
            continue

    origFileName = fileName = "activitats_bcn"
    i = 1
    while os.path.exists(fileName + ".html"):
        fileName = origFileName + "_v%d" % i
        i += 1

    with open(fileName + ".html", "w+") as f:
        f.write("<!DOCTYPE html><html><head><title>LP EVENTS/BICING/PARKING BCN</title>\
           <meta http-equiv=Content-Type content=\"text/html; charset=utf-8\">\
           <style> table { font-family: arial, sans-serif; border-collapse:\
           collapse; width: 100%;}\
           td, th {border: 1px solid #dddddd; text-align: left; padding: 8px;}\
           tr:nth-child(even) { background-color: #dddddd; } </style>\
           <body> <h2>EVENTS</h2> <table><thead>" + headerPlaces +
                "</thead><tbody>" + bodyPlaces + "</tbody></table>")

args = parseParam()
bicings = getBikes()
places = getPlaces(args.lan, args.key)
generateHTML()
