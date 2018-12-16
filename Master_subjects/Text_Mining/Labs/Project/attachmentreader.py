# -*- coding: utf-8 -*-
"""
Created on Mon Jan 22 10:17:38 2018

@author: Carles
"""

from email import message_from_file
import os

# Path to directory where attachments will be stored:
OutputPath = "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/Output"
path = OutputPath

# To have attachments extracted into memory, change behaviour of 2 following functions:

def file_exists (f):
    """Checks whether extracted file was extracted before."""
    return os.path.exists(os.path.join(path, f))

def save_file (fn, cont):
    """Saves cont to a file fn"""
    file = open(os.path.join(path, fn), "wb")
    file.write(cont)
    file.close()

def construct_name (id, fn):
    """Constructs a file name out of messages ID and packed file name"""
    id = id.split(".")
    id = id[0]+id[1]
    return id+"."+fn

def disqo (s):
    """Removes double or single quotations."""
    s = s.strip()
    if s.startswith("'") and s.endswith("'"): return s[1:-1]
    if s.startswith('"') and s.endswith('"'): return s[1:-1]
    return s

def disgra (s):
    """Removes < and > from HTML-like tag or e-mail address or e-mail ID."""
    s = s.strip()
    if s.startswith("<") and s.endswith(">"): return s[1:-1]
    return s

def pullout (m, key):
    """Extracts content from an e-mail message.
    This works for multipart and nested multipart messages too.
    m   -- email.Message() or mailbox.Message()
    key -- Initial message ID (some string)
    Returns tuple(Text, Html, Files, Parts)
    Text  -- All text from all parts.
    Html  -- All HTMLs from all parts
    Files -- Dictionary mapping extracted file to message ID it belongs to.
    Parts -- Number of parts in original message.
    """
    Html = ""
    Text = ""
    Files = {}
    Parts = 0
    if not m.is_multipart():
        if m.get_filename(): # It's an attachment
            fn = m.get_filename()
            cfn = construct_name(key, fn)
            Files[fn] = (cfn, None)
            if file_exists(cfn): return Text, Html, Files, 1
            save_file(cfn, m.get_payload(decode=True))
            return Text, Html, Files, 1
        # Not an attachment!
        # See where this belongs. Text, Html or some other data:
        cp = m.get_content_type()
        if cp=="text/plain": Text += m.get_payload(decode=True)
        elif cp=="text/html": Html += m.get_payload(decode=True)
        else:
            # Something else!
            # Extract a message ID and a file name if there is one:
            # This is some packed file and name is contained in content-type header
            # instead of content-disposition header explicitly
            cp = m.get("content-type")
            try: id = disgra(m.get("content-id"))
            except: id = None
            # Find file name:
            o = cp.find("name=")
            if o==-1: return Text, Html, Files, 1
            ox = cp.find(";", o)
            if ox==-1: ox = None
            o += 5; fn = cp[o:ox]
            fn = disqo(fn)
            cfn = construct_name(key, fn)
            Files[fn] = (cfn, id)
            if file_exists(cfn): return Text, Html, Files, 1
            save_file(cfn, m.get_payload(decode=True))
        return Text, Html, Files, 1
    # This IS a multipart message.
    # So, we iterate over it and call pullout() recursively for each part.
    y = 0
    while 1:
        # If we cannot get the payload, it means we hit the end:
        try:
            pl = m.get_payload(y)
        except: break
        # pl is a new Message object which goes back to pullout
        t, h, f, p = pullout(pl, key)
        Text += t; Html += h; Files.update(f); Parts += p
        y += 1
    return Text, Html, Files, Parts

def extract (msgfile, key):
    """Extracts all data from e-mail, including From, To, etc., and returns it as a dictionary.
    msgfile -- A file-like readable object
    key     -- Some ID string for that particular Message. Can be a file name or anything.
    Returns dict()
    Keys: from, to, subject, date, text, html, parts[, files]
    Key files will be present only when message contained binary files.
    For more see __doc__ for pullout() and caption() functions.
    """
    m = message_from_file(msgfile)
    print(m)
    From, To, Subject, Date = caption(m)
    Text, Html, Files, Parts = pullout(m, key)
    Text = Text.strip(); Html = Html.strip()
    msg = {"subject": Subject, "from": From, "to": To, "date": Date,
        "text": Text, "html": Html, "parts": Parts}
    if Files: msg["files"] = Files
    return msg

def caption (origin):
    """Extracts: To, From, Subject and Date from email.Message() or mailbox.Message()
    origin -- Message() object
    Returns tuple(From, To, Subject, Date)
    If message doesn't contain one/more of them, the empty strings will be returned.
    """
    Date = ""
    if origin.has_key("date"): Date = origin["date"].strip()
    From = ""
    if origin.has_key("from"): From = origin["from"].strip()
    To = ""
    if origin.has_key("to"): To = origin["to"].strip()
    Subject = ""
    if origin.has_key("subject"): Subject = origin["subject"].strip()
    return From, To, Subject, Date


Path = 'C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/Emails/noname_13.eml'

f = open(Path, "rb")
print(extract(f, f.name))
f.close()
type(f.name)




#####################

##########################
import os, re
import email
import argparse
import olefile

def extractAttachment(msg, eml_files, output_path):
    #print len(msg.get_payload())
    #print msg.get_payload()
    if len(msg.get_payload()) > 2:
        if isinstance(msg.get_payload(), str):
            try:
                extractOLEFormat(eml_files, output_path)
            except IOError:
                #print 'Could not process %s. Try manual extraction.' % (eml_files)
                #print '\tHeader of file: %s\n' % (msg.get_payload()[:8])
                pass

        elif isinstance(msg.get_payload(), list):
            count = 0
            while count < len(msg.get_payload()):
                payload = msg.get_payload()[count]
                #récupérer les pièces jointes 
                filename = payload.get_filename()
                #os.rename(filename,'rrrrr'+filename)
                #filename=os.path.join(str(filename), str(eml_files))
                if filename is not None:
                    try:
                        magic = payload.get_payload(decode=True)[:4]
                    except TypeError:
                        magic = "None"                    
                    # Print the magic deader and the filename for reference.
                    printIT(eml_files, magic, filename)
                    # Write the payload out.
                    writeFile(filename, payload, output_path)
                count += 1

    elif len(msg.get_payload()) == 2:
        payload = msg.get_payload()[1]
        filename = payload.get_filename()
        try:
            magic = payload.get_payload(decode=True)[:4]
        except TypeError:
            magic = "None"
        # Print the magic deader and the filename for reference.
        printIT(eml_files, magic, filename)
        # Write the payload out.
        writeFile(filename, payload, output_path)        

    elif len(msg.get_payload()) == 1:
        attachment = msg.get_payload()[0]
        payload = attachment.get_payload()[1]
        filename = attachment.get_payload()[1].get_filename()
        try:
            magic = payload.get_payload(decode=True)[:4]
        except TypeError:
            magic = "None"        
        # Print the magic deader and the filename for reference.
        printIT(eml_files, magic, filename)
        # Write the payload out.
        writeFile(filename, payload, output_path)
    #else:
    #    print 'Could not process %s\t%s' % (eml_files, len(msg.get_payload()))

def extractOLEFormat(eml_files, output_path):
    data = '__substg1.0_37010102'
    filename = olefile.OleFileIO(eml_files)
    msg = olefile.OleFileIO(eml_files)
    attachmentDirs = []
    for directories in msg.listdir():
        if directories[0].startswith('__attach') and directories[0] not in attachmentDirs:
            attachmentDirs.append(directories[0])

    for dir in attachmentDirs:
        filename = [dir, data]
        if isinstance(filename, list):
            filenames = "/".join(filename)
            filename = msg.openstream(dir + '/' + '__substg1.0_3707001F').read().replace('\000', '')


            payload = msg.openstream(filenames).read()
            magic = payload[:4]
            # Print the magic deader and the filename for reference.
            printIT(eml_files, magic, filename)
            # Write the payload out.
            writeOLE(filename, payload, output_path)
#filename = str(eml_files)+"--"+str(filename)
def printIT(eml_files, magic, filename):
    filename = str(eml_files)+"--"+str(filename)
    print ('Email Name: %s\n\tMagic: %s\n\tSaved File as: %s\n' % (eml_files, magic, filename))

def writeFile(filename, payload, output_path):

    filename = str(eml_files)+"--"+str(filename)
    try:
        file_location = output_path + filename
        open(os.path.join(file_location), 'wb').write(payload.get_payload(decode=True))
    except (TypeError, IOError):
        pass

def writeOLE(filename, payload, output_path):
    open(os.path.join(output_path + filename), 'wb')
def main():
    parser = argparse.ArgumentParser(description='Attempt to parse the attachment from EML messages.')
    parser.add_argument('-p', '--path',default='C:\\Users\\hamd\\Desktop\\TEX\\emails' ,help='eml')#Path to EML files
    parser.add_argument('-o', '--out', default='C:\\Users\\hamd\\Desktop\\TEX\\PJ\\eml_files\\',help='pj')#Path to write attachments to.
    args = parser.parse_args()    

    if args.path:
        input_path = args.path
    else:
        print ("You need to specify a path to your EML files.")
        exit(0)

    if args.out:
        output_path = args.out
    else:
        print ("You need to specify a path to write your attachments to.")
        exit(0)

    for root, subdirs, files in os.walk(input_path):
        for file_names in files:
            eml_files = os.path.join(root, file_names)
            msg = email.message_from_file(open(eml_files))
            extractAttachment(msg, eml_files, output_path)

if __name__ == "__main__":
    main()
    
 
    
    
