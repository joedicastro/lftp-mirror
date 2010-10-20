#!/usr/bin/env python
# -*- coding: utf8 -*-

"""
    lftp_mirror.py: This script mirrors a remote FTP server dir with a local dir
"""

#===============================================================================
# This Script mirrors a remote FTP server dir with a local dir (or vice versa)
# and stores a daily compressed copy of local dir
#===============================================================================

#===============================================================================
#    Copyright 2010 joe di castro <joe@joedicastro.com>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#===============================================================================

__author__ = "joe di castro - joe@joedicastro.com"
__license__ = "GNU General Public License version 3"
__date__ = "26/05/2010"
__version__ = "0.4"

try:
    import sys
    import os
    import glob
    import argparse
    import optparse
    import base64
    import time
    import re
    import tarfile
    import pynotify
    import gtk
    import platform
    import socket
    import smtplib
    from subprocess import Popen, PIPE, STDOUT
    from email.mime.text import MIMEText
    from email.mime.multipart import MIMEMultipart
    from email.utils import COMMASPACE, formatdate
except ImportError:
    # Checks the installation of the necessary python modules
    print((os.linesep * 2).join(["An error found importing one module:",
    str(sys.exc_info()[1]), "You need to install it", "Exit..."]))
    sys.exit(-2)

class Logger():
    """

    Create a log object to log script messages.

    These messages can be sended via email or writed in a log file

    """

    def __init__(self):
        """Create the object Logger itself and set a variable.

        This variable is about this python file:

        __script_name = The script name

        """
        self.__log = ''
        self.__script_name = os.path.basename(__file__).split('.')[0]

    def __len__(self):
        return len(self.__log)

    def __format__(self, tit, cont, decor):
        """Format a block or a list of lines to enhance comprehension.

        (str) tit -- title for the block or list
        (str or iterable) cont -- line/s for the list/block content
        ('=' or '_') decor - define if it's list or block and decorate it

        make the looks of self.block() and self.list()

        """
        ending = {'=':'', '_':os.linesep}[decor]
        end = {'=': '=' * 80, '_':''}[decor]
        begin = ' '.join([tit.upper(), (80 - (len(tit) + 1)) * decor]) + ending
        cont = [cont] if isinstance(cont, str) else cont
        sep = os.linesep
        self.__log += sep.join([begin, sep.join(cont), end, sep])

    def block(self, title, content):
        """A block of text lines headed and followed by a line full of '='.

        (str) title -- The title that start the first line of '='
        (str or iterable) content -- The line/s between the '=' lines

        There's not any empty line between the '=' lines and content, e.g.:

        TITLE ==================================================
        content
        ========================================================

        """
        if content:
            self.__format__(title, content, '=')

    def list(self, title, content):
        """A list of text lines headed by a line full of '_'.

        (str) title -- The title that start the line of '_'
        (str or iterable) content -- The line/s after the '_' line

        After the '_' line is a empty line between it and the content, e.g.:

        TITLE __________________________________________________

        content

        """
        if content:
            self.__format__(title, content, '_')

    def free(self, content):
        """Free text unformatted.

        (str) content -- Text free formated

        """
        if isinstance(content, str):
            self.__log += content + os.linesep * 2

    def time(self, title):
        """A self.block() formated line with current time and date.

        (str) title -- Title for self.block()

        Looks like this, the data and time are right-justified:

        TITLE ==================================================
                                       Friday 09/10/10, 20:01:39
        ========================================================

        """
        self.block(title, '{0:>80}'.format(time.strftime('%A %x, %X')))

    def header(self, url, msg):
        """A self.block() formated header for the log info.

        (str) url -- The url of the script
        (str) msg -- Message to show into the header. To Provide any useful info

        It looks like this:

        SCRIPT =================================================
        script name and version
        url

        msg
        ========================================================

        """
        script = '{0} (ver. {1})'.format(self.__script_name, __version__)
        self.block('Script', [script, url, '', msg])

    def get(self):
        """Get the log content."""
        return self.__log

    def send(self, subject, send_from='', dest_to='', mail_server='localhost',
             server_user='', server_pass=''):
        """Send a email with the log.

        Arguments:
            (str) send_from -- a sender's email address (default '')
            (str or list) dest_to -- a list of receivers' email addresses ('')
            (str) subject -- the mail's subject
            (str) mail_server -- the smtp server (default 'localhost')
            (str) server_user -- the smtp server user (default '')
            (str) server_pass --the smtp server password (default '')

        If 'send_from' or 'dest_to' are empty or None, then script user's
        mailbox is assumed instead. Useful for loggin scripts

        """
        local_email = '@'.join([os.getenv('LOGNAME'), socket.gethostname()])
        if not send_from:
            send_from = local_email
        if not dest_to:
            dest_to = [local_email]

        dest_to_addrs = COMMASPACE.join(dest_to) # receivers mails
        message = MIMEMultipart()
        message['Subject'] = '{0} - {1}'.format(subject,
                                                time.strftime('%A %x, %X'))
        message['From'] = send_from
        message['To'] = dest_to_addrs
        message['Date'] = formatdate(localtime=True)
        message.preamble = "You'll not see this in a MIME-aware mail reader.\n"
        message.attach(MIMEText(self.__log))

        # initialize the mail server
        server = smtplib.SMTP()
        # Connect to mail server
        try:
            server.connect(mail_server)
        except socket.gaierror:
            self.list('mail error', 'Wrong server, are you sure is correct?')
        except socket.error:
            self.list('mail error', 'Server unavailable or connection refused')
        # Login in mail server
        if mail_server != 'localhost':
            try:
                server.login(server_user, server_pass)
            except smtplib.SMTPAuthenticationError:
                self.list('mail error', 'Authentication error')
            except smtplib.SMTPException:
                self.list('mail error', 'No suitable authentication method')
        # Send mail
        try:
            server.sendmail(send_from, dest_to_addrs, message.as_string())
        except smtplib.SMTPRecipientsRefused:
            self.list('mail error', 'All recipients were refused.'
                      'Nobody got the mail.')
        except smtplib.SMTPSenderRefused:
            self.list('mail error', 'The server didn’t accept the from_addr')
        except smtplib.SMTPDataError:
            self.list('mail error', 'An unexpected error code, Data refused')
        # Disconnect from server
        server.quit()

    def write(self, append=False):
        """Write the log to a file.

        The name of the file will be like this:

        script.log

        where 'script' is the name of the script file without extension (.py)

        (boolean) append -- If true appends log to file, else writes a new one

        """
        mode = 'ab' if append else 'wb'
        with open('{0}.log'.format(self.__script_name), mode) as log_file:
            log_file.write(self.__log)

def options():
    """Defines the command line arguments and options for the script"""
    usage = """usage: %prog site remote_FTP_dir local_dir [Options]

By default downloads the changes from remote FTP directory to local directory.
To upload changes from local to remote FTP, use the "-r, --reverse" option"""
    desc = "Mirror a remote FTP dir into a local dir or vice versa"
    parser = optparse.OptionParser(usage=usage, version="%prog " + __version__,
                                   description=desc)
    parser.add_option("-u", "--user", dest="user",
                      help="The ftp account user. If no present, "
                      "use the script default", metavar='user')
    parser.add_option("-p", "--pass", dest="password",
                      help="The ftp account password. If no present, "
                      "use the script default", metavar='password')
    parser.add_option("-a", "--anon", action='store_true', dest="anonymous",
                      help="Set user as anonymous. "
                      "Disables user and password options", default=False)
    parser.add_option("-s", "--secure", action='store_const', const='s',
                       dest="secure", default='',
                      help="Uses the sftp protocol instead of ftp")
    parser.add_option("-e", "--erase", action='store_const', const='e',
                      dest="erase", default='',
                      help="Delete files not present at target site")
    parser.add_option("-n", "--newer", action='store_const', const='n',
                      dest="newer", default='',
                      help="Download only newer files")
    parser.add_option("-P", "--parallel", action='store_const', const='P',
                      dest="parallel", default='',
                      help="Download files in parallel")
    parser.add_option("-r", "--reverse", action='store_const', const='R',
                      dest="reverse", default='',
                      help="Reverse, upload files from local to remote")
    parser.add_option("--delete-first", action='store_const',
                      const=' --delete-first',
                      dest="del_first", default='',
                      help="Delete old files before transferring new ones")
    parser.add_option("--depth-first", action='store_const',
                      const=' --depth-first',
                      dest="depth_first", default='',
                      help="Descend into subdirectories, before transfer files")
    parser.add_option("--no-empty-dirs", action='store_const',
                      const=' --no-empty-dirs',
                      dest="no_empty", default='',
                      help="Don't create empty dirs (needs --depth-first)")
    parser.add_option("--no-recursion", action='store_const',
                      const=' --no-recursion',
                      dest="no_recursion", default='',
                      help="Don't go to subdirectories")
    parser.add_option("--dry-run", action='store_const',
                      const=' --dry-run',
                      dest="dry_run", default='',
                      help="Simulation, don't execute anything. Writes to log")
    parser.add_option("--use-cache", action='store_const',
                      const=' --use-cache',
                      dest="cache", default='',
                      help="Use cached directory listings")
    parser.add_option("--del-source", action='store_const',
                      const=' --Remove-source-files',
                      dest="del_source", default='',
                      help="Remove files (no dirs) after transfer (Caution!)")
    parser.add_option("--only-missing", action='store_const',
                      const=' --only-missing',
                      dest="missing", default='',
                      help="Download only missing files")
    parser.add_option("--only-existing", action='store_const',
                      const=' --only-existing',
                      dest="existing", default='',
                      help="Download only files already existing at target")
    parser.add_option("--loop", action='store_const',
                      const=' --loop',
                      dest="loop", default='',
                      help="Loop until no changes found")
    parser.add_option("--ignore-size", action='store_const',
                      const=' --ignore-size',
                      dest="size", default='',
                      help="ignore size when deciding whether to download")
    parser.add_option("--ignore-time", action='store_const',
                      const=' --ignore-time',
                      dest="time", default='',
                      help="ignore time when deciding whether to download")
    parser.add_option("--no-perms", action='store_const',
                      const=' --no-perms',
                      dest="perms", default='',
                      help="don't set file permissions")
    parser.add_option("--no-umask", action='store_const',
                      const=' --no-umask',
                      dest="umask", default='',
                      help="don't apply umask to file modes")
    parser.add_option("--no-symlinks", action='store_const',
                      const=' --no-symlinks',
                      dest="symlinks", default='',
                      help="don't create symbolic links")
    parser.add_option("--allow-suid", action='store_const',
                      const=' --allow-suid',
                      dest="suid", default='',
                      help="set suid/sgid bits according to remote site")
    parser.add_option("--allow-chown", action='store_const',
                      const=' --allow-chown',
                      dest="chown", default='',
                      help="try to set owner and group on files")
    parser.add_option("--dereference", action='store_const',
                      const=' --dereference',
                      dest="dereference", default='',
                      help="download symbolic links as files")

    parser.add_option("-q", "--quiet", action='store_true', dest='quiet',
                       help="The detailed mirror process is no "
                       "displayed, but is added to the log.", default=False)
    parser.add_option("--no-email", action='store_true', dest='no_email',
                       help="No sends email with the log", default=False)
    parser.add_option("--no-compress", action='store_true', dest='no_compress',
                      help="Don't create daily archive files", default=False)
    return parser

def check_execs_posix_win(*progs):
    """Check if the programs are installed.

    Returns two values:

    (dict) windows_paths - a dictionary of executables/paths (keys/values)
    (boolean) is_windows - True it's a Windows OS, False it's a *nix OS

    """
    def not_found(app):
        """ If executable is not installed, exit and report."""
        msg = 'The {0} program is necessary to run this script'.format(app)
        sys.exit(msg)

    windows_paths = {}
    is_windows = True if platform.system() == 'Windows' else False
    # get all the drive unit letters if the OS is Windows
    windows_drives = re.findall(r'(\w:)\\',
                                Popen('fsutil fsinfo drives', stdout=PIPE).
                                communicate()[0]) if is_windows else None
    for prog in progs:
        if is_windows:
            # Set all commands to search the executable in all drives
            win_cmds = ['dir /B /S {0}\*{1}.exe'.format(letter, prog) for
                        letter in windows_drives]
            # Get the first location (usually in C:) of the all founded where
            # the executable exists
            exe_paths = (''.join([Popen(cmd, stdout=PIPE, stderr=PIPE,
                                        shell=True).communicate()[0] for
                                        cmd in win_cmds])).split(os.linesep)[0]
            # Assign the path to the executable or report not found if empty
            windows_paths[prog] = exe_paths if exe_paths else not_found(prog)
        else:
            try:
                Popen([prog, '--help'], stdout=PIPE, stderr=PIPE)
            except OSError:
                not_found(prog)
    return windows_paths, is_windows

def notify(msg, status):
    """Send notification status messages through libnotify.

    Paramaters:

    (str) msg -- The message to display into notification
    (str) status -- Type of notification status (ok|info|error|warm|ask|sync)

    """
    if not pynotify.is_initted():
        pynotify.init('lftp_mirror')
    note = pynotify.Notification("LFTP Mirror", msg)
    helper = gtk.Button()
    icons = {'ok':gtk.STOCK_YES, 'info':gtk.STOCK_DIALOG_INFO,
             'error':gtk.STOCK_DIALOG_ERROR, 'warm':gtk.STOCK_DIALOG_WARNING,
             'ask':gtk.STOCK_DIALOG_QUESTION, 'sync':gtk.STOCK_JUMP_TO}
    icon = helper.render_icon(icons[status], gtk.ICON_SIZE_BUTTON)
    note.set_icon_from_pixbuf(icon)
    note.show()
    return

def bes_unit_size(f_size):
    """Get a size in bytes and convert it for the best unit for readability.

    Return two values:

    (int) bu_size -- Size of the path converted to the best unit for easy read
    (str) unit -- The units (IEC) for bu_size (from bytes(2^0) to YiB(2^80))

    """
    for exp in range(0, 90 , 10):
        bu_size = f_size / pow(2.0, exp)
        if int(bu_size) < 2 ** 10:
            unit = {0:'bytes', 10:'KiB', 20:'MiB', 30:'GiB', 40:'TiB', 50:'PiB',
                    60:'EiB', 70:'ZiB', 80:'YiB'}[exp]
            break
    return {'s':bu_size, 'u':unit}

def get_size(the_path):
    """Get size of a directory tree or a file in bytes."""
    path_size = 0
    if os.path.isfile(the_path):
        path_size = os.path.getsize(the_path)
    for path, dirs, files in os.walk(the_path):
        for fil in files:
            filename = os.path.join(path, fil)
            path_size += os.path.getsize(filename)
    return path_size

def compress(r_path):
    """Compress the local directory into a gz file.

    Creates a file for each weekday, an removes the old files if exists"""
    output = ''
    old_gzs = glob.glob('*_{0}.tar.gz'.format(time.strftime('%a')))
    r_dir = re.findall('(?:.*\/)?(.*)', r_path)[0]
    name = "{0}_{1}.tar.gz".format(r_dir, time.strftime('%d%b%Y_%H:%M_%a'))
    gz_file = tarfile.open(name, "w:gz")
    gz_file.add(r_path, arcname=r_dir)
    gz_file.close()
    output = 'Created file{1}{0}{1}'.format(os.path.join(r_path, name),
                                            os.linesep)
    for gz_f in old_gzs:
        os.remove(gz_f)
        output += 'Deleted old file {0}{1}'.format(gz_f, os.linesep)
    return output


def main():
    """Main section"""

#===============================================================================
# SCRIPT PARAMATERS TO EXECUTE THE SCRIPT AS A PROGRAMMED TASK
#===============================================================================

    # ftp user name ('user' by default)
    script_user = 'user'
    # ftp password, with a minimum security measure, encoded by base64
    # ('password' by default)
    script_password = base64.b64decode('cGFzc3dvcmQ=')
    # ftp server, name or ip ('localhost' by default)
    script_site = 'localhost'
    # ftp directory
    script_ftp_dir = 'directory'
    # local directory
    script_local_dir = '/your/path/to/your/local/directory/'

#===============================================================================
# END PARAMETERS
#===============================================================================

    # first, parse the options & arguments
    (opts, args) = options().parse_args()

    # this script can be executed in two ways:
    #
    #    a) interactive
    #    b) as a programmed task
    #
    # In the first way, you need to give the args (site, remote FTP dir and
    # local dir) to the script in the command line. Options, obviously, are
    # optional.
    #
    # As a programmed task, don't need the args. Takes the scripts variables
    # (previously defined) as default args. Also takes user and password options
    # from the script variables. The other options are optional.

    # set the args and some options for the two ways
    if not args:
        site = script_site
        ftp_dir = script_ftp_dir
        local_dir = os.path.normpath(script_local_dir)
        user_pass = '{0} {1}'.format(script_user, script_password)
    else:
        site = args[0]
        ftp_dir = args[1]
        local_dir = os.path.normpath(args[2])
        user_pass = '{0} {1}'.format(opts.user, opts.password)


    user = {True:'', False:user_pass}[opts.anonymous]
    protocol = '{0}ftp'.format(opts.secure)
    verbose = {True:STDOUT, False:None}[opts.quiet]
    # end

    # changes to the local dir
    os.chdir('{0}/..'.format(local_dir))

    # initalize the log
    log = Logger()

    # log the header
    url = 'http://code.joedicastro.com/lftp-mirror'
    msg = 'connected to {0}{1}Mirror {2} to {3}'.format(site, os.linesep,
                                                        ftp_dir, local_dir)
    log.header(url, msg)

    # log Start Time
    log.time('Start time')

    notify('Mirroring with {0}...'.format(script_site), 'sync')

    # create the script file to import with lftp
    sc_opts = ('-vvv' + opts.erase + opts.newer + opts.parallel + opts.reverse +
               opts.del_first + opts.depth_first + opts.no_empty +
               opts.no_recursion + opts.dry_run + opts.cache + opts.del_source +
               opts.missing + opts.existing + opts.loop + opts.size + opts.time
               + opts.perms + opts.umask + opts.symlinks + opts.suid +
               opts.chown + opts.dereference)

    with open('ftpscript', 'w') as script:
        script.write('open {0}://{1}{2}'.format(protocol, site, os.linesep))
        script.write('user {0}{1}'.format(user, os.linesep))
        script.write('mirror {0} {1} {2}{3}'.format(sc_opts, ftp_dir, local_dir,
                                                    os.linesep))
        script.write('exit{0}'.format(os.linesep))
    # end

    # mirror the directories
    cmd = ['lftp', '-d', '-f', script.name]
    sync = Popen(cmd, stdout=PIPE, stderr=verbose)
    # end mirroring

    # adds the lftp output to the log
    result = sync.stdout.readlines()
    lftp_log = ''
    for line in result:
        lftp_log += line
    log.list('lftp output', lftp_log)
    # end

    # compress the dir and create a .gz file with date
    if not opts.reverse and not opts.no_compress:
        notify('Compressing folder...', 'info')
        log.list('Rotate compressed copies', compress(local_dir))
    # end compress

    log.time('End Time')
    size = bes_unit_size(get_size(os.path.join(local_dir, '..')))
    log.block('Disk space used', '{0:>76.2f} {1}'.format(size['s'], size['u']))

    # send the log by mail and write the log file
    if not opts.no_email:
        log.send('FTP Sync')
    log.write(True)

    # remove the script file
    os.remove(script.name)

    notify('Ended Ok', 'ok')

if __name__ == "__main__":
    check_execs_posix_win('lftp') # Check first if lftp is installed
    main()

