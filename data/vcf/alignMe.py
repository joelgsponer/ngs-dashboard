#!/usr/bin/python

#25.01.2012 This script is intended to pipeline the alignement of sequencing data on the workhorse workstation
#Created by Joel Gsponer joel.gsponer@unibas.ch


import os
import sys
import subprocess
import sqlite3
import datetime



referenceFolder = os.path.expanduser("~/Documents/ReferenceGenome/")
nameReferenceGenome = ("hg19bwaindex")
logDatabase = 'AlignmentDatabase.db'
progressMailAddress = 'joel.gsponer@unibas.ch'


def fncOpenWelcomeText():
	f = open("Welcome.txt", "r")
	text = f.read()
	print text
	f.close()

def fncDatabaseOpen():
	conn = sqlite3.connect(logDatabase)
	c = conn.cursor()
	return c, conn

def fncCreateDatabase():
	print ("-> Check if Log Database exists - "), 
	if os.path.exists(logDatabase) == True:
		print "OK"
	else:
		print "NO"
		print ("-> Creating database - "),
		conn = sqlite3.connect(logDatabase)
		c = conn.cursor()
		c.execute("create table if not exists tblFastafiles (fastaindex INTEGER PRIMARY KEY, fastaname TEXT, timestamp DATE,bwaVersion TEXT, exitcode INTEGER)")
		conn.commit()
		conn.close()
		print "OK"
		
def fncSendMail(fasta, step, exitcode, start, end):
	print ("\n-> Sending progress mail to %s - " %progressMailAddress),
	try:
		tempMail = open("./temp.mail", 'wb')
		tempMail.write("Hello")
		tempMail.write("File: %s has finishided %s with exitcode %s" %(fasta, step, exitcode))
		tempMail.write("Start: %s"%start)
		tempMail.write("End: %s"%end)
		tempMail.close()
		os.subprocess.call(["mailx", "-s %s" %fasta, progressMailAddress])
		print "OK"
	except:
		print "FAILED"
		
if __name__=="__main__":
	#Welcome Banner
	fncOpenWelcomeText()
	#LogDatabase handling
	fncCreateDatabase()
	db = fncDatabaseOpen()
	#Paths
	targetFolder = os.path.expanduser(raw_input("-> Plese enter a TARGET-folder: ")) + "/"
	#Checking Path
	print ("-> Checking TARGET-folder - "),
	if os.path.isdir(targetFolder) == True:
		print "OK\n\n"
		print"---------------------------------------------------------"
	else: 
		print "ERROR - Folder does not exist or isn't reachable"
		sys.exit(1)
	#Search suitable files
	filesInFolder = os.listdir(targetFolder)
	fastaFiles = []
	for file in filesInFolder:
		if file.endswith("fastq") or file.endswith("fastq.gz"):
			print ("\n-> Aviable FASTA files:")
			print file
			fastaFiles.append(file)
    #Align with BWA 
	print ("\n\n-> Checking BWA version - "), 
	try:
		bwaversion = subprocess.check_output(["which", "bwa"])
	except:
		bwaversion = "ERROR not possible to determine BWA version"
	print bwaversion
	
	for fasta in fastaFiles:
		try:
			print("\n-> Perform BWA alignment for %s - " %(str(fasta))),
			startTime = str(datetime.datetime.now())
			saiFile = open(targetFolder + fasta + ".sai", "wb")
			exitcode = subprocess.call(["bwa", "aln", referenceFolder + nameReferenceGenome, targetFolder + file], stdout = saiFile)
			#exitcode = subprocess.call(["echo", "Hello"])# <- to be removed one it is moved to WorkHorse 
			saiFile.close()
			print("OK")  
		except:
			print "ERROR - Alignment for %s failed!" %fasta
		endTime = str(datetime.datetime.now())
		fncSendMail(fasta, "BWA aln",exitcode, startTime,endTime)
		#Write to database
		db[0].execute('INSERT INTO tblFastafiles (fastaname, timestamp,bwaVersion, exitcode) VALUES (?,?,?,?)', [fasta, str(datetime.datetime.now()),bwaversion, exitcode] )
		db[1].commit()
	
	#tidy up
	print ("\n-> Closing database - "),
	try:
		db[1].close()
		print "OK"
	except:
		print "WARNING database alredy closed"
	 
	sys.exit(0)
