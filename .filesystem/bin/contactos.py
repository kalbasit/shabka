#!/usr/bin/env python
import sys
import os
import re
import pickle
from datetime import datetime

from gdata.contacts.service import ContactsService, ContactsQuery


class GooBook(object):
    def __init__ (self, username, password, max_results, cache_filename):
        self.username = username
        self.password = password
        self.max_results = max_results
        self.cache_filename = cache_filename
        self.addrbk = {}

    def query(self, query):
        """
        Do the query, and print it out in
        """
        self.load()
        match = re.compile(query, re.I).search
        resultados = dict([(k,v) for k,v in self.addrbk.items() if match(k) or match(v)])
        for (k,v) in resultados.items():
            print "%s\t%s"%(k,v)

    def load(self):
        """
        Load the cached addressbook feed, or fetch it (again) if it is
        old or missing or invalid or anyting
        """
        try:
            picklefile = file(self.cache_filename, 'rb')
        except IOError:
            # we should probably catch picke errors too...
            self.fetch()
            #  simplifico el feed, con formato 'titulo'\t'email' sin ''
        else:
            stamp, self.addrbk = pickle.load(picklefile) #optimizar
            if (datetime.now() - stamp).days:
                self.fetch()
        finally:
            self.store()


    def fetch(self):
        """
        Actually go out on the wire and fetch the addressbook.

        """
        client = ContactsService()
        client.ClientLogin(self.username, self.password)
        query = ContactsQuery()
        query.max_results = self.max_results
        feed = client.GetContactsFeed(query.ToUri())
        for e in feed.entry:
            for i in e.email:
                if e.title.text:
                    self.addrbk[i.address] = e.title.text
                else:
                    self.addrbk[i.address] = i.address

    def store(self):
        """
        Pickle the addressbook and a timestamp
        """
        picklefile = file(self.cache_filename, 'wb')
        stamp = datetime.now()
        pickle.dump((stamp, self.addrbk), picklefile)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        sys.exit(1)

    try:
        from settings import USERNAME, PASSWORD, MAX_RESULTS, CACHE_FILENAME
    except ImportError:
        raise RuntimeError("Please create a valid settings.py"
                           " (look at settings_example.py for inspiration)")
    else:
        CACHE_FILENAME = os.path.realpath(os.path.expanduser(CACHE_FILENAME))

    goobk = GooBook(USERNAME, PASSWORD, MAX_RESULTS, CACHE_FILENAME)
    goobk.query(sys.argv[1])
