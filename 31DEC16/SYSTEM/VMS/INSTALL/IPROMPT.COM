$!IPROMPT.COM
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
$!---------------------------------------------------------------------
$!  This routine prompts for all AIPS parameters.
$!  OUTPUTS:
$!    PARM'I'    STR   The names of the system parameters.
$!    QUEST'I'   STR   A description of the system parameters.
$!    TOTQUEST   INT   The number of system parameters (start with 1).
$!    'PARMi'    ???   The values of the system parameters.
$!---------------------------------------------------------------------
$!
$                                   ! Initialization and defaults.
$ QUEST1 :== "OLD VERSION                                   "
$ PARM1 :== "OLDVER"
$ OLDVER == "NONE"
$ PTYPE1 == "S"
$!
$ QUEST2 :== "NEW VERSION                                   "
$ PARM2 :== "NEWVER"
$ NEWVER == "NONE"
$ PTYPE2 == "S"
$!
$ QUEST3 :== "TST VERSION                                   "
$ PARM3 :== "TSTVER"
$ TSTVER == "NONE"
$ PTYPE3 == "S"
$!
$ QUEST4 :== "LOCAL SYSTEM NAME                             "
$ PARM4 :== "SYSNAME"
$ SYSNAME == "Unknown VAX site"
$ PTYPE4 == "S"
$!
$ QUEST5 :== "AP; 1=FPS 120B, 2=FPS 5000, 3=NONE            "
$ PARM5 :== "APTYPE"
$ UpperLim5 = 3
$ LowerLim5 = 1
$ PTYPE5 == "I"
$!
$ QUEST6 :== "DIRECTORY CONTAINING AP LIBRARIES             "
$ PARM6 :== "APDIR"
$ PTYPE6 == "S"
$!
$ QUEST7 :== "NO. OF DATA DISK DRIVES                       "
$ PARM7 == "NUMDISKS"
$ NUMDISKS == 1
$ UpperLim7 = 9
$ LowerLim7 = 1
$ PTYPE7 == "I"
$!
$ QUEST8 :== "NO OF TAPE DRIVES                             "
$ PARM8 == "NUMTAPES"
$ NUMTAPES == 1
$ UpperLim8 = 9
$ LowerLim8 = 1
$ PTYPE8 == "I"
$!
$ QUEST9 :== "NO OF INTERACTIVE AIPS                        "
$ PARM9 == "NUMINTERAIPS"
$ NUMINTERAIPS == 3
$ UpperLim9 = 12
$ LowerLim9 = 1
$ PTYPE9 == "I"
$!
$ QUEST10 :== "NO OF BATCH QUEUES                            "
$ PARM10 == "NUMBATCH"
$ NUMBATCH == 0
$ UpperLim10 = 4
$ LowerLim10 = 0
$ PTYPE10 == "I"
$!
$ QUEST11 :== "NO ENTRIES IN PRIVATE CATALOGS                "
$ PARM11 == "CATENTRIES"
$ CATENTRIES == 300
$ UpperLim11 =  1000
$ LowerLim11 = -1000
$ PTYPE11 == "I"
$!
$ QUEST12 :== "MAXIMUM ALLOWED USER NUMBER                   "
$ PARM12 == "MAXUSERNUM"
$ MAXUSERNUM == 1500
$ UpperLim12 = 4095
$ LowerLim12 = 10
$ PTYPE12 == "I"
$!
$ QUEST13 :== "NO OF LINES PER CRT PAGE                      "
$ PARM13 == "LINESPERCRT"
$ LINESPERCRT == 24
$ UpperLim13 = 100
$ LowerLim13 = 4
$ PTYPE13 == "I"
$!
$ QUEST14 :== "NO OF LINES PER PRINT PAGE                    "
$ PARM14 == "LINESPERPRINTER"
$ LINESPERPRINTER == 61
$ UpperLim14 = 500
$ LowerLim14 = 4
$ PTYPE14 == "I"
$!
$ QUEST15 :== "PLOTTER NO OF X DOTS PER PAGE                 "
$ PARM15 == "PLOTXDOTS"
$ PLOTXDOTS == 2112
$ UpperLim15 = 10000
$ LowerLim15 = 10
$ PTYPE15 == "I"
$!
$ QUEST16 :== "PLOTTER NO OF Y DOTS PER PAGE                 "
$ PARM16 == "PLOTYDOTS"
$ PLOTYDOTS == 1600
$ UpperLim16 = 10000
$ LowerLim16 = 10
$ PTYPE16 == "I"
$!
$ QUEST17 :== "PLOTTER NO OF X DOTS PER CHARACTER            "
$ PARM17 == "XDOTSCHAR"
$ XDOTSCHAR == 20
$ UpperLim17 = 10000
$ LowerLim17 = 2
$ PTYPE17 == "I"
$!
$ QUEST18 :== "PLOTTER NO OF Y DOTS PER CHARACTER            "
$ PARM18 == "YDOTSCHAR"
$ YDOTSCHAR == 25
$ UpperLim18 = 10000
$ LowerLim18 = 2
$ PTYPE18 == "I"
$!
$ QUEST19 :== "PLOTTER NO OF X DOTS PER MM                   "
$ PARM19 :== "PLOTXDOTSMM"
$ PLOTXDOTSMM == "7.83"
$ PTYPE19 == "F"
$!
$ QUEST20 :== "PLOTTER NO OF Y DOTS PER MM                   "
$ PARM20 :== "PLOTYDOTSMM"
$ PLOTYDOTSMM == "7.83"
$ PTYPE20 == "F"
$!
$ QUEST21 :== "NO OF WORDS IN AP (IN 1024 S)                 "
$ PARM21 == "WORDSINAP"
$ WORDSINAP == 64
$ UpperLim21 = 256
$ LowerLim21 = 1
$ PTYPE21 == "I"
$!
$ QUEST22 :== "NO OF GRAPHICS TERMINALS AVAILABLE            "
$ PARM22 == "NUMTEKS"
$ NUMTEKS == 1
$ UpperLim22 = 9
$ LowerLim22 = 0
$ PTYPE22 == "I"
$!
$ QUEST23 :== "NO OF TV DEVICES AVAILABLE                    "
$ PARM23 == "NUMTVS"
$ NUMTVS == 1
$ UpperLim23 = 9
$ LowerLim23 = 0
$ PTYPE23 == "I"
$!
$ QUEST24 :== "NO OF RESERVED AIPS TERMINALS                 "
$ PARM24 == "NUMRESTT"
$ NUMRESTT == 0
$ UpperLim24 = 9
$ LowerLim24 = 0
$ PTYPE24 == "I"
$!
$ Num1stGrp = 24
$                              ! Init some important vaules to NULL.
$ TVTYPE == 0
$ APTYPE == 0
$ BATOUT == ""
$ PLOTQUE == ""
$ QMSQ == ""
$ SYSNAME == ""
$ THISVER == ""
$ APDIR == ""
$ I = 0
$ INITLOOP:
$    I = I + 1
$    AIPSDA0'I' == ""
$    AIPSMT0'I' == ""
$    RTT'I' == ""
$    TTT'I' == ""
$    TKTT'I' == ""
$    TV'I' == ""
$ if (I.lt.9) then GOTO INITLOOP
$ temp = f$search("ilasttime.com")
$ if (temp.eqs."") then GOTO STARTQUEST
$   inquire I -
      "ENTER; 1 = Use Default Values, 2 = Use Values I entered last time"
$   if (I.eq.2) then @iLastTime
$ STARTQUEST:
$                              ! Ask for the tape density
$   write sys$output " "
$   TAPEDENS1:
$      write sys$output "ENTER THE TAPE DENSITY: 1600, 6250"
$      inquire Temp "ENTER"
$      if (Temp .eqs. "1600") then GOTO TAPEDENS2
$      if (Temp .eqs. "6250") then GOTO TAPEDENS2
$      GOTO TAPEDENS1
$   TAPEDENS2:
$      TAPEDENS :== 'Temp'
$                              ! Ask for the computer descriptor.
$ if (SYSNAME.nes."") then GOTO ENDSYSNAME
$   write sys$output " "
$   ID:
$     write sys$output -
        "ENTER A 20 CHARACTER STRING IDENTIFYING YOUR LOCAL SYSTEM"
$     inquire Temp "ENTER"
$   if (Temp .eqs. "") then GOTO ID
$   SYSNAME :== 'Temp'
$ ENDSYSNAME:
$!
$ if (THISVER.nes."") then GOTO ENDVER
$ write sys$output " "
$ VERSION:
$   inquire Temp "ARE YOU INSTALLING THIS VERSION AS 1=NEW OR 2=TST ?"
$   if (Temp.eq.1) then THISVER == "NEW"
$   if (Temp.eq.2) then THISVER == "TST"
$ if (THISVER .eqs. "") then GOTO VERSION
$ ENDVER:
$ if (THISVER.eqs."NEW") then NEWVER :== 'VERDATE'
$ if (THISVER.eqs."TST") then TSTVER :== 'VERDATE'
$ if (NEWVER.nes."NONE") then GOTO APSTUFF
$    inquire Temp "ENTER NEW AREA AS 15MMMYY"
$    if (Temp.nes."") then NEWVER == Temp
$ APSTUFF:
$ if (APTYPE.ne.0) then GOTO MENU1
$ write sys$output " "
$ APTYPEQ:
$   inquire Temp "ENTER AP TYPE; 1=FPS 120B, 2=FPS 5000 SERIES, 3=NONE"
$ if (Temp.lt.1) .or. (Temp.gt.3) then GOTO APTYPEQ
$ APTYPE == Temp
$ MENU1:
$ write sys$output " "
$ write sys$output "AIPS Configuration Menu"
$ I = 5
$ if (APTYPE.eq.3) then GOTO ENDAP
$   I = 6
$   if (APDIR.nes."") then GOTO ENDAP
$   APDIR == f$logical ("FPS")
$   if (APDIR.nes."") then GOTO ENDAP
$     inquire Temp "ENTER DIRECTORY FOR AP LIBRARIES"
$     APDIR == Temp
$ ENDAP:
$ @iQUEST 1 'I'
$ if (QUESTRETURN.eq.-2) then GOTO MENU1
$ if ((APTYPE.eq.3) .or. (APDIR.nes."")) then GOTO MENU2
$   inquire Temp "ENTER DIRECTORY FOR AP LIBRARIES"
$   APDIR == Temp
$ MENU2:
$ write sys$output " "
$ write sys$output "System Parameter Menu"
$                              ! Ask QUESTions for this screen full
$                              ! and get PARMs.
$ @iQUEST 7 'Num1stGrp'
$ if QUESTRETURN.eq.-2 then GOTO MENU1
$!
$ write sys$output " "
$ tq = Num1stGrp
$ nocheck = ""
$ if (NUMTEKS.lt.1).and.(NUMTVS.lt.1) then GOTO FINI
$ if (NUMTEKS.lt.1) then GOTO ENDGRAPH
$                              ! Storage tube (TEK Style) graphics device.
$    tq = tq + 1
$    QUEST'tq' :== "NO OF USERS ALLOWED ACCESS TO TKS             "
$    PARM'tq' == "NUMTKACC"
$    PTYPE'tq' == "I"
$    if (F$TYPE(numtkacc).eqs."") then NUMTKACC == NUMINTERAIPS
$    UpperLim'tq' = 6
$    LowerLim'tq' = 0
$    tq = tq + 1
$    QUEST'tq' :== "NO OF X DOTS PER MM ON GRAPHICS               "
$    PARM'tq' :== "XDOTSTK"
$    if (F$TYPE(xdotstk).eqs."") then XDOTSTK == "5.00"
$    PTYPE'tq' == "F"
$!
$ ENDGRAPH:
$ write sys$output " "
$ if (NUMTVS.lt.1) then GOTO ENDTV
$    if (TVTYPE.ne.0) then GOTO GOTTV
$       TVTYPELOOP:
$          write sys$output -
        "Enter TV Type; 1=IIS M70, 2=IIS M75, 3=DEANZA, 4=COMTAL,"
$          inquire TV -
        "Enter TV Type; 5=IVAS, 6=ARGS"
$       if (TV.lt.1) .or. (TV.gt.6) then GOTO TVTYPELOOP
$       TVTYPE == TV
$    GOTTV:
$                              ! Raster (TV) graphics device.
$    tq = tq + 1
$    QUEST'tq' :== "NO OF USERS ALLOWED ACCESS TO TVS             "
$    if (F$TYPE(numtvacc).eqs."") then NUMTVACC == NUMINTERAIPS
$    PARM'tq' == "NUMTVACC"
$    PTYPE'tq' :== "I"
$    UpperLim'tq' = 6
$    LowerLim'tq' = 1
$    tq = tq + 1
$    QUEST'tq' :== "TV; 1=IIS M70, 2=M75, 3=DEANZA 4=COMTAL 5=IVAS 6=ARGS"
$    PARM'tq' == "TVTYPE"
$    PTYPE'tq' :== "I"
$    UpperLim'tq' = 6
$    LowerLim'tq' = 1
$    I = 1
$    TVPLANES:
$      if (I.gt.NUMTVS) then GOTO ENDTV
$      tq = tq + 1
$      QUEST'tq' :== "NUMBER OF IMAGE PLANES IN TV DEVICE ''I'         "
$      PARM'tq' :== TV'I'IPLANES
$      PTYPE'tq' :== "I"
$      if (F$TYPE(tv'I'iplanes).eqs."") then TV'I'IPLANES == 4
$      UpperLim'tq' = 32
$      LowerLim'tq' = 1
$      tq = tq + 1
$      QUEST'tq' :== "NUMBER OF GRAPHICS PLANES IN TV DEVICE ''I'      "
$      PARM'tq' :== TV'I'GPLANES
$      PTYPE'tq' :== "I"
$      if (F$TYPE(tv'I'gplanes).eqs."") then TV'I'GPLANES == 4
$      UpperLim'tq' = 32
$      LowerLim'tq' = 1
$      I = I + 1
$    GOTO TVPLANES
$ ENDTV:
$ MENU3:
$ I = Num1stGrp + 1
$ write sys$output " "
$ write sys$output "Graphics Parameters Menu"
$ @iQUEST 'I' 'tq'
$ if QUESTRETURN.eq.-2 then GOTO MENU2
$ FINI:
$ TOTQUEST == tq
$!@@@                             Get all of the device names.
$ write sys$output " "
$ I = 0
$ NumQUEST = 100
$ DISKLOOP0:
$   I = I + 1
$   if (I.gt.NUMDISKS) then GOTO ENDDISK0
$   NumQUEST = NumQUEST + 1
$   qtemp = "AIPS disk ''I' Data area (Device and Directory)  "
$   QUEST'NumQUEST' = qtemp
$   PARM'NumQUEST' := AIPSDA0'I'
$   PTYPE'NumQuest := "S"
$   if (AIPSDA0'I' .nes. "") then GOTO DISKLOOP0
$     inquire Temp "ENTER ''qtemp'"
$     AIPSDA0'I' == Temp
$ GOTO DISKLOOP0
$ ENDDISK0:
$ AIPSDA00 == AIPSDA01
$ I = 0
$ TAPELOOP0:
$   I = I + 1
$   if (I.gt.NUMTAPES) then GOTO ENDTAPE0
$   NumQUEST = NumQUEST + 1
$   qtemp ="AIPS tape drive ''I' physical name               "
$   QUEST'NumQUEST' = qtemp
$   PARM'NumQUEST' := AIPSMT0'I'
$   PTYPE'NumQuest := "S"
$   if (AIPSMT0'I' .nes. "") then GOTO TAPELOOP0
$      inquire Temp "ENTER ''qtemp'"
$      AIPSMT0'I' == Temp
$      @iAddCol AIPSMT0'I'
$ GOTO TAPELOOP0
$ ENDTAPE0:
$ I = 0
$ RESSTTLOOP0:
$   I = I + 1
$   if (I.gt.NUMRESTT) then GOTO ENDRESSTT0
$   NumQUEST = NumQUEST + 1
$   PARM'NumQUEST' := RTT'I'
$   PTYPE'NumQuest := "S"
$   qtemp = "Terminal reserved for AIPS''I'                   "
$   QUEST'NumQUEST' = qtemp
$   if (RTT'I'.nes."") then GOTO GOTRTT
$      inquire temp "ENTER ''qtemp'"
$      RTT'I' == temp
$      @iAddCol RTT'I'
$   GOTRTT:
$   NumQUEST = NumQUEST + 1
$   PARM'NumQUEST' := TTT'I'
$   PTYPE'NumQuest := "S"
$   qtemp = "Message terminal (CR if none) for AIPS''I'       "
$   QUEST'NumQUEST' = qtemp
$   if (TTT'I'.nes."") then GOTO RESSTTLOOP0
$      inquire temp "ENTER ''qtemp'"
$      TTT'I' == temp
$      @iAddCol TTT'I'
$ GOTO RESSTTLOOP0
$ ENDRESSTT0:
$ if (NUMBATCH.eq.0) then GOTO ENDBATCH0
$   NumQUEST = NumQUEST + 1
$   PARM'NumQUEST' = "BATOUT"
$   PTYPE'NumQuest := "S"
$   qtemp = "Batch output terminal or file                 "
$   QUEST'NumQUEST' = qtemp
$   if (BATOUT.nes."") then GOTO ENDBATCH0
$      inquire temp "ENTER ''qtemp'"
$      BATOUT == Temp
$ ENDBATCH0:
$ I = 0
$ TEKTKLOOP0:
$   I = I + 1
$   if (I.gt.NUMTEKS) then GOTO ENDTEKTK0
$   NumQUEST = NumQUEST + 1
$   PARM'NumQUEST' := TKTT'I'
$   PTYPE'NumQuest := "S"
$   qtemp = "physical name for 4012 type graphics display ''I'"
$   QUEST'NumQUEST' = qtemp
$   if (TKTT'I'.nes."") then GOTO TEKTKLOOP0
$      inquire temp "ENTER ''qtemp'"
$      TKTT'I' == Temp
$      @iAddCol TKTT'I'
$ GOTO TEKTKLOOP0
$ ENDTEKTK0:
$ I = 0
$ TVLOOP0:
$   I = I + 1
$   if (I.gt.NUMTVS) then GOTO ENDTV0
$   NumQUEST = NumQUEST + 1
$   PARM'NumQUEST' := TV'I'
$   PTYPE'NumQuest := "S"
$   qtemp = "Physical name for TV device ''I'                 "
$   QUEST'NumQUEST' = qtemp
$   if (TV'I'.nes."") then GOTO TVLOOP0
$      inquire temp "ENTER ''qtemp'"
$      TV'I' == temp
$      @iAddCol TV'I'
$ GOTO TVLOOP0
$ ENDTV0:
$ NumQUEST = NumQUEST + 1
$ PARM'NumQUEST' = "PLOTQUE"
$ PTYPE'NumQuest := "S"
$ qtemp = "Plot queue for plotter (CR if none)           "
$ QUEST'NumQUEST' = qtemp
$ if (PLOTQUE.nes."") then GOTO GOTPLOT
$    inquire temp "ENTER ''qtemp'"
$    PLOTQUE == temp
$ GOTPLOT:
$ NumQUEST = NumQUEST + 1
$ PARM'NumQUEST' = "QMSQ"
$ PTYPE'NumQuest := "S"
$ qtemp = "Queue name for laser printer (CR if none)     "
$ QUEST'NumQUEST' = qtemp
$ if (QMSQ.nes."") then GOTO GOTQMS
$    inquire temp "ENTER ''qtemp'"
$    QMSQ == temp
$ GOTQMS:
$ write sys$output " "
$ write sys$output "Device Name Menu"
$ @iQUEST 101 'NumQUEST'
$ if QUESTRETURN.eq.-2 then GOTO MENU3
$!
$                                  ! Save all entries
$ @iSaveAns
$ EXIT
