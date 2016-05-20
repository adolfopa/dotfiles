#! /usr/bin/python

from __future__ import print_function

import argparse
import collections
import itertools
import os
import re
import shutil
import sys
import xml.etree.ElementTree as ET

def const(x):
    return lambda y: x

def negate(f):
    return lambda x: not f(x)

def is_iml(fname):
    return fname.endswith('.iml')

RX_IGNORABLE = re.compile('^(?:\\..*|src|lib|build|classes)')

def is_ignorable(fname):
    return re.match(RX_IGNORABLE, fname)

def walk(dn, keep=const(True), recur=const(True)):
    for fn in os.listdir(dn):
        fname = os.path.join(dn, fn)

        if keep(fn):
            yield fname

        if os.path.isdir(fname) and recur(fn):
            for child in walk(fname, keep, recur):
                yield child

def backup(fname):
    if os.path.isfile(fname):
        shutil.copy(fname, fname + '~')

CORE_MODULES = collections.OrderedDict({
    'portal.client': 'portal-client',
    'portal.impl': 'portal-impl',
    'portal.kernel': 'portal-kernel',
    'portal.test.integration': 'portal-test-integration',
    'portal.test': 'portal-test',
    'support.resin': 'support-resin',
    'support.tomcat': 'support-tomcat',
    'support.websphere': 'support-websphere',
    'util.bridges': 'util-bridges',
    'util.java': 'util-java',
    'util.taglib': 'util-taglib'
})

def fix_iml(fname):
    def core_module(url):
        for (k, v) in CORE_MODULES.items():
            if k in url:
                return v

    def core_references(document):
        for elt in document.findall(".//orderEntry[@type='module-library']"):
            lib = elt.find('./library')
            root = lib.find('./CLASSES/root')
            url = root.get('url')

            module = core_module(url)

            if module:
                yield (module, elt, lib)

    document = ET.parse(fname)

    count = 0

    for (module, elt, lib) in core_references(document):
        elt.set('type', 'module')
        elt.set('module-name', module)
        elt.set('scope', 'PROVIDED')

        elt.remove(lib)

        count = count + 1

    if count > 0:
        backup(fname)
        document.write(fname)

    return count

def parse_cmdline():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        '--src',
        dest='src',
        default=os.getcwd(),
        metavar='DIR',
        help='Liferay source tree root directory'
    )

    parser.add_argument(
        '--patch-iml',
        action='store_true',
        dest='iml',
        help='replace core module references in .iml files (disabled by default)'
    )

    parser.add_argument(
        '--no-patch-iml',
        action='store_false',
        dest='iml',
        help='do NOT modify any of the .iml files (default behaviour)'
    )

    parser.add_argument(
        '--project-file',
        dest='prj',
        metavar='FILE',
        help='location of IntelliJ project file'
    )

    parser.add_argument(
        '--dwim',
        action='store_true',
        dest='dwim',
        help='Do What I Mean: patch imls and try to guess root src and project file locations'
    )

    parser.set_defaults(iml=False)

    return parser.parse_args()

def module_conf(pfile, fname):
    if not pfile:
        return ('', '')

    parts = itertools.dropwhile(
        lambda s: s <> 'modules', fname.split('/'))

    group = '/'.join(list(parts)[1:-2]) or 'monolith'

    prefix = os.path.commonprefix([pfile, fname])

    file_path = os.path.join('$PROJECT_DIR$', fname[len(prefix):])

    return (file_path, group)

def xml_header(out):
    print('<?xml version="1.0" encoding="UTF-8"?>', file=out)
    print('<project version="4">', file=out)
    print('  <component name="ProjectModuleManager">', file=out)
    print('    <modules>', file=out)

def xml_footer(out):
    print('    </modules>', file=out)
    print('  </component>', file=out)
    print('</project>', file=out)

MODULE_XML_TEMPLATE = (
    '      <module fileurl="file://{0}" filepath="{0}" group="{1}" />'
)

def dwim(opts):
    def find_file_location(dname, fname, stop_at):
        while dname <> stop_at:
            path = os.path.join(dname, fname)

            if (os.path.exists(path)):
                return dname

            dname = os.path.dirname(dname)

    home = os.getenv('HOME')

    src_root = find_file_location(os.getcwd(), 'build.properties', stop_at=home) or opts.src
    idea_dir = find_file_location(src_root, '.idea', stop_at=home) or opts.prj

    opts.src = src_root
    opts.prj = os.path.join(idea_dir, '.idea', 'modules.xml')
    opts.iml = True

    print("Found project at {0}, using source root {1}.".format(opts.prj, opts.src))

    return opts

if __name__ == "__main__":
    opts = parse_cmdline()

    if opts.dwim:
        dwim(opts);

    if opts.prj:
        opts.prj = os.path.abspath(opts.prj)
        backup(opts.prj)

    with open(opts.prj or os.devnull, mode='w') as pf:
        if opts.prj:
            xml_header(pf)

        src = os.path.abspath(opts.src)

        for fn in walk(src, keep=is_iml, recur=negate(is_ignorable)):
            if opts.iml:
                n = fix_iml(fn)

                if n > 0:
                    print('Updated {0} references in {1}'.format(n, fn))

            (iml_path, iml_group) = module_conf(opts.prj, fn)

            print(MODULE_XML_TEMPLATE.format(iml_path, iml_group), file=pf)

        if opts.prj:
            xml_footer(pf)
