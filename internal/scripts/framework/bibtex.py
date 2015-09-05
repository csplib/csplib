# -*- coding: utf-8 -*-
from pybtex.database.input import bibtex
import re
from calendar import month_name
import codecs
import latexcodec  #needed
import sys
from pprint  import pprint, pformat

if sys.version_info[0] == 3:
    from io import StringIO
else:
    from StringIO import StringIO

_months = {
    'jan': 1, 'feb': 2, 'mar': 3, 'apr': 4, 'may': 5, 'jun': 6,
    'jul': 7, 'aug': 8, 'sep': 9, 'oct': 10, 'nov': 11, 'dec': 12,
}


def add_filters(template_env):
    template_env.filters['author_fmt'] = _author_fmt
    template_env.filters['author_list'] = _author_list
    template_env.filters['title'] = _title
    template_env.filters['venue_type'] = _venue_type
    template_env.filters['venue'] = _venue
    template_env.filters['main_url'] = _main_url
    template_env.filters['extra_urls'] = _extra_urls
    template_env.filters['monthname'] = _month_name
    template_env.filters['publisher'] = _publisher



class Bib(object):
    def __init__(self, bibfile):
        super(Bib, self).__init__()
        self.bibfile = bibfile
        self.db=None

    def parse(self):
        with codecs.open(self.bibfile, encoding="latex+utf8") as ff:
            # remove {} useful in TeX, not in html
            f=re.sub(u"{(\w)}", u"\\1", ff.read(), re.UNICODE)
            buf=StringIO(f)
            db = bibtex.Parser().parse_stream(buf)
        for k, v in db.entries.items():
            v.fields['key'] = k
            # fragment is the bibtex key sanitised for use in html anchors
            fragment = re.sub("[^\w]", "_", k)
            fragment = re.sub("^(\d+)", "_\\1", fragment)
            v.fields['fragment'] = fragment

        def _sortkey(entry):
            e = entry.fields
            try:
                year = '{:04d}'.format(int(e['year']))
            except KeyError:
                return "000000"

            try:
                monthnum = _month_match(e['month'])
                year += '{:02d}'.format(monthnum)
            except KeyError:
                year += '00'
            return year

        self.db = db

    def to_html(self, apply_template):
        if not self.db:
            self.parse()

        def _sortkey(entry):
            e = entry.fields
            try:
                year = '%04d'% (int(e['year']))
            except KeyError:
                return "000000"

            try:
                monthnum = _month_match(e['month'])
                year += '%02d' % monthnum
            except KeyError:
                year += '00'
            return year

        bib_sorted = sorted(self.db.entries.values(), key=_sortkey, reverse=True)
        html = apply_template("bib_format.html", entries=bib_sorted )
        return html

    def __repr__(self):
        return "%s(%s)" % (self.__class__, pformat(self.__dict__))




def _author_fmt(author):
    return u' '.join(author.first() + author.middle() + author.last())


def _andlist(ss, sep=', ', seplast=', and ', septwo=' and '):
    if len(ss) <= 1:
        return ''.join(ss)
    elif len(ss) == 2:
        return septwo.join(ss)
    else:
        return sep.join(ss[:-1]) + seplast + ss[-1]


def _author_list(authors):
    return _andlist(list(map(_author_fmt, authors))).replace("{","").replace("}","")


def _venue_type(entry):
    venuetype = ''
    if entry.type == 'inbook':
        venuetype = 'Chapter in '
    elif entry.type == 'techreport':
        venuetype = 'Technical Report '
    elif entry.type == 'phdthesis':
        venuetype = 'Ph.D. thesis, {}'.format(entry.fields['school'])
    return venuetype


def _venue(entry):
    f = entry.fields
    venue = ''
    if entry.type == 'article':
        venue = f.get('journal', '')
        try:
            if f['volume'] and f['number']:
                venue += ' {0}({1})'.format(f['volume'], f['number'])
        except KeyError:
            pass
    elif entry.type in {'inproceedings', 'incollection'}:
        venue = f['booktitle']
        # Not sure I want the series
        # try:
        #     if f['series']:
        #         venue += ' ({})'.format(f['series'])
        # except KeyError:
        #     pass
    elif entry.type == 'inbook':
        venue = f['title']
    elif entry.type == 'techreport':
        venue = ", ".join(filter(lambda a: a, [ f.get(k, "") for k in ['number', 'institution'] ]))
    elif entry.type == 'phdthesis':
        venue = ''
    else:
        # venue = 'Unknown venue (type={})'.format(entry.type)
        venue =""
    return venue

def _publisher(entry):
    if entry.type == 'book':
        try:
            return entry.fields['publisher']
        except KeyError:
            return ""
    else:
        return ""

def _title(entry):
    if entry.type == 'inbook':
        title = entry.fields['chapter']
    else:
        title = entry.fields['title']

    # remove {} useful in TeX, not in html
    # title = re.sub("[{}]", "", title)
    # If there are '$', there is maths, and we will not try to remove {}
    if '$' not in title:
        title = re.sub('(?<!\\\)[{}]',"",title)
    return title


def _main_url(entry):
    urlfields = ('url', 'ee')
    for f in urlfields:
        if f in entry.fields:
            return entry.fields[f]
    if 'doi' in entry.fields:
        return 'http://dx.doi.org/' + entry.fields['doi']
    return None


def _extra_urls(entry):
    urls = {}
    for k, v in entry.fields.items():
        if k != 'howpublished':
            if k.endswith('_url'):
                k = k[:-4]
            else:
                continue

        url = v
        urlmatch = re.match(r"\s*\\url\s*{(.*)}\s*", url)
        if urlmatch:
            url = urlmatch.group(1)
        urltype = "{}: {}".format(k, url)
        urls[urltype] = url
    return urls


def _month_match(mon):
    if re.match('^[0-9]+$', mon):
        return int(mon)
    return _months[mon.lower()[:3]]


def _month_name(monthnum):
    try:
        return month_name[int(monthnum)]
    except:
        return ''
