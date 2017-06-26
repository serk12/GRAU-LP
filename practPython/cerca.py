#!/usr/bin/python3
# -*- coding: utf-8 -*-

import sys
import csv
from math import radians, cos, sin, atan2, sqrt
import urllib.request
import xml.etree.ElementTree as ET
import unicodedata
import os
import codecs
import string

def addData(data, inc):
    d,m,a = data.split('/')
    if inc:
        if (int(m) == 2 and int(d) == 28) or (int(d) == 31 and int(m) in [1,3,5,7,8,10,12]) or (int(d) == 30 and int(m) in[4,6,9,11]):
            d = '1'
            if m == '12':
                m = '1'
                a = str(int(a)+1)
            else:
                m = str(int(m)+1)
        else:
            d = str(int(d)+1)
    else:
        if int(d) == 1:
            if int(m) == 1:
                m = '12'
                d = '31'
                a = str(int(a)-1)
            else:
                m = str(int(m)-1)
                if int(m) == 2:
                    d = '28'
                elif int(m) in[4,6,9,11]:
                    d = '30'
                else:
                    d = '31'
        else:
            d = str(int(d)-1)
    return d + '/' + m + '/' + a

# [] === AND        () === OR
def makeExprBool(consult, isTime):
    if isinstance(consult, str):
        return "'%s' in str" % consult
    elif len (consult) > 0:
        if isTime and isinstance(consult, tuple):
            data, minD, maxD = consult
            dataAux = data
            index = minD
            expression = "(" + data
            while index < maxD:
                inc = index >= 0
                data = addData(data, inc)
                index += 1
                if index != 0: expression += " or " + data
                else: data = dataAux
        else:
            op = "or" if isinstance(consult, tuple) else "and"
            expression = "("
            first = True
            for elem in consult:
                if first: first = False
                else: expression += " %s " % op
                expression += makeExprBool(elem, isTime)
        return expression + ")"
    else: return "(True)"

class Params:
    def __init__(self, typeConsult1, consult1, typeConsult2='null', consult2=[]):
        if typeConsult1 == 'key':
            self.key = makeExprBool(consult1, False)
            self.date = makeExprBool(consult2, True)
        elif typeConsult1 == 'date':
            self.date = makeExprBool(consult1, True)
            self.key = makeExprBool(consult2, False)
        else:
            self.date = makeExprBool([], True)
            self.key  = makeExprBool([], False)

    def evalConditions(self, str, date):
        condDate = self.date == "(True)" or (date in self.date)
        condKeys = eval(self.key)
        return condDate and condKeys

class Position:
    RADI_TERRA = 6371000
    def __init__(self, lat, lon):
        self.latitud, self.longitud = map(radians, (float(lat), float(lon)))

    def distance(self, point):
        dlon = self.longitud - point.longitud
        dlat = self.latitud  - point.latitud

        a = sin(dlat/2)**2 + cos(point.longitud)*cos(self.longitud)*sin(dlon/2)**2
        c = 2*atan2(sqrt(a), sqrt(1 - a))

        return self.RADI_TERRA*c

class BicingStop:
    def __init__(self, pos, street, num, qttyBic, slots):
        self.pos, self.street, self.num, self.qttyBic, self.slots = (pos, street, num, int(qttyBic), int(slots))
    
    def hasSpots(self):
        return self.slots > 0

    def hasBikes(self):
        return self.qttyBic > 0

def adapt(input_str):
    nfkd_form = unicodedata.normalize('NFKD', input_str)
    return u"".join([c for c in nfkd_form if not unicodedata.combining(c)]).lower()

class Parking:
    def __init__(self, address, gmapx, gmapy):
        self.address = address
        self.pos     = Position(gmapx, gmapy)
class Event:
    def __init__(self, begindate, enddate, address, city, name, pos, proxhour, district, barri):
        self.begindate, self.enddate, self.address, self.city, self.name, self.pos, self.proxhour, self.district, self.barri = \
        (begindate, enddate, address, city, name, pos, proxhour, district, barri)

    def getStringSearch(self):
        return adapt(self.name + " " + self.district + " " + self.barri + " " + self.address)

    def getStreet(self): 
        return self.address + " " + self.barri + " " + self.city
    
    def getProximalBike(self, bikes, dist): #refactorizar
        bikesClose = []
        for aux in bikes:
            if self.pos.distance(aux.pos) < dist and aux.hasBikes():
                bikesClose.append(aux)
        return bikesClose
    
    def getProximalSlot(self, slots, dist):
        slotsClose = []
        for aux in slots:
            if self.pos.distance(aux.pos) < dist and aux.hasSpots():
                slotsClose.append(aux)
        return slotsClose

    def getProximalPark(self, parks, dist):
        parkingClose = []
        for aux in parks:
            if self.pos.distance(aux.pos) < dist:
                parkingClose.append(aux)
        return parkingClose

def parseParam():
    size = len(sys.argv)
    if size == 1:
        return Params('null', ['null'])
    if size == 3 or size == 5:
        isNotKey = '--key' not in sys.argv
        isNotDate = '--date' not in sys.argv
        if (isNotKey and isNotDate) or ((len(sys.argv) == 5) and (isNotKey or isNotDate)):
            print ("Error: bad flags")
            sys.exit()
        try:
            if size == 3:
                return Params(sys.argv[1][2:], eval(sys.argv[2]))
            else:
                return Params(sys.argv[1][2:], eval(sys.argv[2]), sys.argv[3][2:], eval(sys.argv[4]))
        except:
            print ("Error: bad tipe values")
            sys.exit()
    else:
        print ("Usage: %s consult bicing" % sys.argv[0])
        sys.exit()

def getXmlTree(url):
    try:
        fileXml = urllib.request.urlopen(url)
    except:
        print ("No conection to internet")
        sys.exit()
    xmlString = fileXml.read()
    fileXml.close()
    return ET.fromstring(xmlString)

def getBicingStations():
    URL_BICING = "http://wservice.viabicing.cat/getstations.php?v=1"
    xmlTree = getXmlTree(URL_BICING)

    STATUS = "OPN"
    COLUMNS = ("lat", "long", "street", "streetNumber", "bikes", "slots", "status")
    if xmlTree == None: return []
    stopsBicing = []
    for i in range(1, len(xmlTree)):
        station = xmlTree[i]
        lat, lon, street, num, qttyBic, slots, estat = map(lambda x: station.findtext(x), COLUMNS)
        if estat == STATUS and (int(qttyBic) > 0 or int(slots) > 0):
            stopsBicing.append(BicingStop(Position(lat, lon), street, num, qttyBic, slots))
    return stopsBicing

def getParkings():
    URL_PARK = "http://www.bcn.cat/tercerlloc/Aparcaments.xml"
    xmlTree = getXmlTree(URL_PARK)
    
    TAG_ERROR = "error" 
    TAGS = ("address", "gmapx", "gmapy")
    TAG_PATH = "search/queryresponse/list/list_items/row"
    if xmlTree == None or xmlTree.find(TAG_ERROR) or xmlTree.find(TAG_PATH) == None: return []
    parkings = []
    for aux in xmlTree.findall(TAG_PATH):
        for station in aux:
            address, mapx, mapy = map(lambda x: station.findtext(x), TAGS)
            parkings.append(Parking(address, mapx, mapy))
    return parkings

def getEvent():
    URL_EVENTS = "http://www.bcn.cat/tercerlloc/agenda_cultural.xml"
    treeXml = getXmlTree(URL_EVENTS)

    TAG_ERROR = "error"
    TAG_PATH = "search/queryresponse/list/list_items/row"
    TAGS = ("begindate", "enddate", "address", "city", "name", "gmapx", "gmapy", "proxhour", "district", "addresses/item/barri")
    if treeXml == None or treeXml.find(TAG_ERROR) != None or treeXml.find(TAG_PATH) == None: return []
    events = []
    for aux in treeXml.findall(TAG_PATH):
        for tagEvent in aux:
            try:
                begindate, enddate, address, city, name, gmapx, gmapy, proxhour, district, barri = map(lambda x: tagEvent.findtext(x),  TAGS)
                event = Event(begindate, enddate, address, city, name, Position(gmapx, gmapy), proxhour, district, barri)
                if params.evalConditions(event.getStringSearch(), event.begindate): events.append(event)
            except: continue #ugly 
    return events

def convertListToString(list):
    return u''.join(str(id(aux)) + "<br>" for aux in list) 

def generateHTML():
    idsBici = set({})
    idsPark = set({})
    bodyEvents = ""
    headerEvents = "<tr><th>Acte</th><th>Data</th><th>Adreça</th><th>ID espai disponible</th><th>ID bicing disponible</th><th>ID parking</th></tr>"
    for event in events:
        auxIdsBici = event.getProximalBike(bicings, 500)
        idsBici |= set(auxIdsBici)
        auxIdsSlot = event.getProximalSlot(bicings, 500)
        idsBici |= set(auxIdsSlot)
        auxIdsPark = event.getProximalPark(parkings, 500)
        idsPark |= set(auxIdsPark)
        try:
            bodyEvents += "<tr><td>" + event.name + "</td><td>" + event.begindate + "</td><td>" + event.getStreet() + "</td><td>"+ \
            convertListToString(auxIdsBici) + "</td><td>" + convertListToString(auxIdsSlot) + "</td><td>" + convertListToString(auxIdsPark) \
            + "</td></tr>"
        except: continue #ugly

    headerBicings = "<tr><th>ID parking disponible</th><th>Adreça</th><th>Slots</th><th>Bicis</th><tr>"
    bodyBicings = ""
    for bike in idsBici:
        bodyBicings += "<tr><td>" + str(id(bike)) + "</td><td>" + bike.street + " " + str(bike.num) + "</td><td>" \
        + str(bike.slots) + "</td><td>" + str(bike.qttyBic) + "</td></tr>"

    headerParkings = "<tr><th>ID parking disponible</th><th>Adreça</th></tr>"
    bodyParkings = ""
    for parking in idsPark:
        bodyParkings += "<tr><td>" + str(id(parking)) + "</td><td>" + parking.address + "</td></tr>"

    origFileName = fileName = "activitats_bcn"
    i = 1
    while os.path.exists(fileName + ".html"):
        fileName = origFileName + "_v%d" % i
        i += 1

    with open(fileName + ".html", "w+") as f:
        f.write("<!DOCTYPE html><html><head><title>LP EVENTS/BICING/PARKING BCN</title>\
           <meta http-equiv=Content-Type content=\"text/html; charset=utf-8\">\
           <style> table { font-family: arial, sans-serif; border-collapse: collapse; width: 100%;}\
           td, th {border: 1px solid #dddddd; text-align: left; padding: 8px;}\
           tr:nth-child(even) { background-color: #dddddd; } </style>\
           <body>\
           <h2>EVENTS</h2>\
           <table><thead>" + headerEvents + "</thead><tbody>" + bodyEvents + "</tbody></table>\
           <h2>BICING</h2>\
           <table><thead>" + headerBicings + "</thead><tbody>" + bodyBicings + "</tbody></table>\
           <h2>PARKING</h2>\
           <table><thead>" + headerParkings + "</thead><tbody>" + bodyParkings + "</tbody></table>\
           ")


params = parseParam()

print ("getting bikes...")
bicings = getBicingStations()

print ("getting events...")
events = getEvent()

print ("getting parking...")
parkings = getParkings()

print ("genereting file HTML...")
generateHTML()

##########  300 LINES OF PYTHON ACHIVEMENT UNLOCKED #####################