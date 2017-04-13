$!==========================================================================
$!----------------------------------------------------------------------
$!;  Copyright (C) 1995
$!;  Associated Universities, Inc. Washington DC, USA.
$!;
$!;  This program is free software; you can redistribute it and/or
$!;  modify it under the terms of the GNU General Public License as
$!;  published by the Free Software Foundation; either version 2 of
$!;  the License, or (at your option) any later version.
$!;
$!;  This program is distributed in the hope that it will be useful,
$!;  but WITHOUT ANY WARRANTY; without even the implied warranty of
$!;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
$!;  GNU General Public License for more details.
$!;
$!;  You should have received a copy of the GNU General Public
$!;  License along with this program; if not, write to the Free
$!;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
$!;  MA 02139, USA.
$!;
$!;  Correspondence concerning AIPS should be addressed as follows:
$!;         Internet email: aipsmail@nrao.edu.
$!;         Postal address: AIPS Project Office
$!;                         National Radio Astronomy Observatory
$!;                         520 Edgemont Road
$!;                         Charlottesville, VA 22903-2475 USA
$!----------------------------------------------------------------------
$!
$! MAIL-REMINDER    Utility to remind people of new mail on exploder systems
$!
$!      Overview:  This procedure is intended to be run in batch mode.
$!      ---------  It makes the ASSUMPTION that you have set aside the VMS
$!      MAIL part of an account - or preferably a captive account - to
$!      receive messages from somewhere.  The most frequent use of such
$!      a captive account is to serve as a bulletin board, such as for
$!      the NRAO's AIPS/BANANA electronic mail bulletins.  This command
$!      procedure is a "perpetual" one in that it resubmits itself for
$!      sometime in the future.
$!
$!      What you would like to do, then, is have an automatic way of
$!      filing incoming messages away, say in a file separate from the
$!      main MAIL.MAI, perhaps in a different folder, or maybe both.
$!      This procedure will not only take care of that, but it will
$!      also periodically shoot out a VMS mail message to specified
$!      users reminding them of new messages since the last time the
$!      procedure was run.  This reminder contains a list of the mail
$!      messages similar to what you get from the VMS MAIL's DIRECTORY
$!      command.
$!
$!      Implementation:  You need to specify the following pieces of
$!      ---------------  information:
$!
$!        (1) a short interval in days, e.g. "+1-" for one day;
$!        (2) a mail file, if other than MAIL.MAI, and folder;
$!            if given, it will automatically file new mail there;
$!            otherwise it just files it in folder MAIL;
$!        (3) a second interval, e.g. 7 days, when it will shoot out a
$!            summary of mail in the file/folder to a distribution list;
$!            This MUST be a multiple of the short interval; and
$!        (4) a distribution list for the summary messages.
$!
$!      With this information, the system will do two things.  First, every
$!      short interval, it will scan for new mail in the account it is
$!      running under, and if there is any, it will select it and file it in
$!      the MAIL or other specified file/folder.  Secondly, every long
$!      interval, it selects the MAIL or other specified file/folder, and
$!      produces a summary (via the DIRECTORY command of VMS MAIL) of all
$!      messages in it that arrived in the previous long interval.  It then
$!      sends this summary to the people on the distribution list.
$!      The distribution list is expected to contain ONE name per line.
$!      Blank lines and comments (delimiter "!") are permitted in it.
$!
$!      Restrictions:  The recommended short period is one day; the long
$!      -------------  period is specified as an integer multiple of short
$!      periods.  The suggested (and default) values for these periods are
$!      1 day and 7 days, respectively.  If changing these, be sure that
$!      the short interval is specified in VMS DELTA time and in DAYS
$!      only, i.e. "+n-" where n is the number of days; the procedure
$!      combines it with an absolute time and also performs arithmetic on
$!      it.  The long interval must always be an integer (not a string).
$!      This procedure automatically resubmits itself in the default
$!      batch queue with the /AFTER qualifier and the time specified is
$!      on the hour prior to the current time found at procedure start.
$!      In other words, if you submit the job at 7:45 AM with a short
$!      period of one day, it will be resubmitted for 7:00 AM the
$!      following day.
$!
$!      Disclaimers:  This procedure is written in VAX/VMS DCL (VMS 4.4).
$!      ------------
$!      Copyright (C) by Patrick P. Murphy and the National Radio Astronomy
$!                       Observatory.  All rights reserved.
$!
$!      This software is in the public domain and may be freely copied and
$!      distributed, provided the copyright and this notice is propagated
$!      EXACTLY as it appears between the two lines of equal signs.
$!      If you have to add comments, please do so after this notice.
$!      NRAO and/or Patrick P. Murphy assume NO responsibility for bugs,
$!      errors, and other deficiencies in this code.  It is distributed "as
$!      is", with NO support.  However, if errors are detected, we do ask
$!      that we be notified of them as soon as possible so we can attempt to
$!      rectify them.  Suggestions for improvement are also welcome.
$!
$!      Pat Murphy, National Radio Astronomy Observatory,
$!                  P.O. Box "O", Socorro, NM 87801, USA.    (505) 772-4337
$!
$!             Bitnet: PMURPHY@NRAO            SPAN/HEPNET: 6654::PMURPHY
$!      Arpa/Internet: PMURPHY@NRAO.ARPA       uucp: ...seismo!nrao1!pmurphy
$!==========================================================================
$!
$!       Define the parameters needed.
$!       If you are going to change the time and/or mail parameters,
$!       the following five lines are ALL you should have to change.
$!
$ Short_Interval = "+1-"            ! Delta time (one day), MUST be in DAYS!
$ LONG_INTERVAL  = 7                ! 7 times short interval
$ Mail_File      = "BANANAS"        ! Use "MAIL" for default
$ Mail_Folder    = "BANANAS"        ! Use "MAIL" for default
$ Distrib_List   = "BANANAS.DIS"    ! List of who to send summary to
$!
$! Set the Delete command - interactive is assumed to be debugging mode
$!
$ Delete = "Delete/NoConfirm/NoLog"
$ if f$mode() .eqs. "INTERACTIVE" then Delete = "! Don't delete "
$!
$! (note that VMS MAIL behaves differently in batch and interactive modes!)
$!
$!    Resubmit this thing for whenever ("When" MUST be in quotes)
$!
$ Set NoOn
$ Set Message/Facility/ID/Severity/Text
$ Start_Time = f$extract(11, 3, f$cvtime()) + "00" ! on the hour only
$ When = Start_Time + Short_Interval
$ ThisProc = f$environment("Procedure")
$ MyName = f$parse(ThisProc,,,"Name")      ! Get rid of version# in filespec
$ MyType = f$parse(ThisProc,,,"Type")      ! so new versions get used
$ ThisProc = f$parse(ThisProc,,,"Device") + f$parse(ThisProc,,,"Directory") -
                                          + Myname
$ Purge 'ThisProc'.log/keep=2
$ Submit/Log='ThisProc'.log/After="''When'"/NoPrint 'ThisProc''MyType'
$!
$!    Read the "Number of Short Intervals" file and increment it
$!
$ ShortIntFile = MyName + ".Int"
$ If f$search(ShortIntFile) .nes. "" then goto INT_EXISTS
$   Create 'ShortIntFile'
0
$INT_EXISTS:
$ Open/Read SIF 'ShortIntFile'
$ Read SIF Number
$ Close SIF
$ Delete 'ShortIntFile';*                  ! Get rid of old version
$ Number = Number + 1
$ If Number .gt. Long_Interval then Number = 1
$ Create 'ShortIntFile'                    ! Create new version
$ Open/Append SIF 'ShortIntFile'           ! This way we get standard format
$ Write SIF Number
$ Close SIF
$ Do_Long_Stuff = (Number .eq. 1)
$!
$!   First, do the stuff that needs done every short interval.
$!
$ Temp_File_Root = "Sys$Scratch:" + MyName
$ Temp_File = Temp_File_Root + ".NewStuff"
$ if f$search(Temp_File) .nes. "" then Delete 'Temp_File';*
$ Define/user sys$output 'temp_file'    ! This is a dummy file
$ Mail                                  ! Start up the VMS mail utility
Show New_Mail_Count                     ! See if there is anything new
Exit                                    ! exit from VMS mail
$!
$! Now get the number of new mail messages from this file
$!
$ Open/read NewMail 'Temp_File'
$ Read NewMail Line
$ Close NewMail
$ Delete 'Temp_File';*
$ Write sys$output Line            ! so we can see it in the batch log file
$ Mail_Count = f$element(2, " ", Line)
$ If Mail_Count .eq. 0 then goto LONG_STUFF
$!
$! There are some new messages so put them in the right file/folder
$! Note that there is no attempt at sorting by subject matter here!
$! That would take a lot more work and is not appropriate for DCL.
$!
$   Temp_File = Temp_File_Root + ".Filer"
$   Create 'Temp_File'
$   Open/Append Filer 'Temp_File'
$   Write Filer "$ Mail"
$   Write Filer "Select NewMail"
$   Write Filer "File/all " + Mail_folder + " " + Mail_file
$   Write Filer "Read/New"              ! To reset system's mail counter
$   Write Filer "Exit"                  !  - otherwise it gets confused by this
$   Close Filer
$   @'Temp_File'
$   Delete 'Temp_File';*
$!
$LONG_STUFF:
$!
$ If .not. Do_Long_Stuff then EXIT
$!
$! Now do the long-interval stuff: First, get a list of things since last time
$!
$ Temp_File  = Temp_File_Root + ".Lister"
$ Temp_File2 = Temp_File_Root + ".Output"
$!
$ NumDays = Long_Interval * f$int(Short_Interval - "+" - "-")
$ If (NumDays .le. 0) then NumDays = Long_Interval
$ When = f$cvtime("''Start_Time'-''NumDays'-", "Absolute")
$!
$! Create a temporary command file so we can get a MAIL directory listing
$!
$ Create 'Temp_File'
$ Open/Append Lister 'Temp_File'
$ Write Lister "$ Define/User_Mode sys$output " + Temp_File2
$ Write Lister "$ MAIL"
$ Write Lister "Set file " + Mail_File
$ Write Lister "Directory/Since=""''When'"" " + Mail_Folder
$ Write Lister "Exit"
$ Close Lister
$ @'Temp_File'
$ Delete 'Temp_File';*
$!
$! Open the temporary file generated and see if there was anything there.
$!
$ If f$search(Temp_File2) .eqs. "" then EXIT ! Nothing else to do.
$!
$ Open/Read Index 'Temp_File2'
$ Read Index Line
$ If Line .nes. "%MAIL-E-NOMSGS, no messages" then goto SOME_MESSAGES
$   Close Index
$   Delete 'Temp_File2';*
$   EXIT                                        ! Nothing more to do
$!
$! Now create a message file for distribution
$!
$SOME_MESSAGES:
$ Today = f$extract(0, 17, f$time())
$ Msg_File = Temp_File_Root + ".Message"
$ Create 'Msg_File'
$ Open/Append Msg 'Msg_File'
$ Write Msg "From ''MyName' at ''Today'"
$ Write Msg "The ''Mail_folder' folder of the ''mail_file' file has " + -
            "new message(s)"
$ Write Msg " - here is a summary:"
$ Write Msg ""
$ Num_Msgs = 0
$!
$! Loop through the output listing until we hit the end or null line
$! (does batch mail do dir listings by page?  If so could be a problem)
$!
$LOOP:
$   Read/End=ENDLOOP Index Line
$   If Line .eqs. "" then goto ENDLOOP
$   Write Msg Line
$   Num_Msgs = Num_Msgs + 1
$   Goto LOOP
$ENDLOOP:
$ Write Msg " "
$!
$! If you define a system-wide logical name pointing at the mail file,
$! you can modify these lines for your system, and uncomment them.
$!
$! Write Msg "You can access these from any VLA VAX from your OWN account"
$! Write Msg "by entering MAIL and typing SET FILE BANANA_MAIL."
$!
$ Close Msg
$ Close Index
$ Delete 'Temp_File2';*
$!
$! Allow for header and blank line following; then get Number of messages
$!
$ Num_Msgs = Num_Msgs - 2
$ If Num_Msgs .le. 0 then Delete 'Msg_File';*
$ If Num_Msgs .le. 0 then EXIT
$!
$ Banner = Mail_Folder + " bulletin - " + f$string(Num_Msgs) + " new "
$ If Num_Msgs .eq. 1 then Banner = Banner + "message"
$ If Num_Msgs .ne. 1 then Banner = Banner + "messages"
$!
$! We Now have the message file.  Get the disrtibution file
$!
$ If f$search(Distrib_List) .eqs. "" then EXIT ! no-one cares......
$ Open/Read Dist 'Distrib_List'
$!
$! Read through the distribution list one name at a time.  I know, I
$! could easily ship the whole thing to MAIL, but I prefer to keep on
$! going if one or more names fail (node down or whatever).
$!
$DIST_LOOP:
$   Read/End=ENDISTLOOP Dist UserName
$   UserName = f$edit(UserName, "Trim, Collapse, UnComment, UpCase")
$   if UserName .eqs. "" then goto DIST_LOOP
$     Set NoOn
$     Mail/Subject="''banner'" 'Msg_File' 'UserName'
$     Goto DIST_LOOP
$ENDISTLOOP:
$ Close Dist
$ Delete 'Msg_File';*
$ EXIT
