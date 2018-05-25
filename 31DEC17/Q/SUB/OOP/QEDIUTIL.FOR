C   Edit Class utility module (does all the real editing)
C-----------------------------------------------------------------------
C! UV data and SN/CL table editing with the TV
C# Task AP OOP UV EDITING TV-APPL CALIBRATION
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2003-2004, 2006-2017
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Public functions:
C
C   EDITSN (opcode, object, ierr)
C      edit an SN/CL table edit object   --- OR ---
C      edit a UV data object with an SN/CL table object
C   EDITSY (opcode, object, ierr)
C      edit a SY table edit object   --- OR ---
C      edit a UV data edit object with a SY table object
C   EDITTY (opcode, object, ierr)
C      edit a TY table edit object   --- OR ---
C      edit a UV data edit object with a TY table object
C   EDITUV (opcode, object, ierr)
C      edit UV-data with the data themselves
C-----------------------------------------------------------------------
C   Private functions:
C
C   EDAFND (ant, antens, ierr)
C      find antenna number in list of desired antennas
C   EDANTS (ants, ant1, ant2)
C      Breaks 256*m + n into its parts
C   EDCFND (choice, n, choics, chs)
C      find a string in a list of strings
C   EDDBOX (name, type, mode, chan, corn, ierr)
C      draw a box on the TV
C   EDFCAP (uvdata, ierr)
C      Applies an FC table to a uv data set
C   EDFCDO (ierr)
C      Reapplies FC table's contents to data in core
C   EDFCLI (ierr)
C      lists an FC table's contents
C   EDFCUN (entry, ierr)
C      Undoes one entry in the FC table
C   EDFGAP (uvdata, ierr)
C      Applies a uv data set FG table to table data now in core
C   EDFIND (mode, wtype, ian, type, tvxy, x, it, ir, ierr)
C      find the nearest plotted sample to the TV cursor in TVXY
C   EDFLAG (op, ian, type, flgtim, flgflx, nfl, ierr)
C      flag samples in specified range, remove them from plot and
C      toplot, put them on flagged plot (if any)
C   EDFLAI (type, corn, first, tvxy, tvco, flgtim, flgflx, tvbutt, ierr)
C      interactive display of cursor position to set a flagging box
C   EDFLAR (ierr)
C      interactive flagging of an area: time range and flux range
C   EDFLFA (ierr)
C      interactive flagging of high fluxes
C   EDFLFB (ierr)
C      interactive flagging of low fluxes
C   EDFLPT (ierr)
C      interactive flagging of points (uses Y and X position)
C   EDFLQU (ierr)
C      interactive flagging of points in quick mode
C   EDFLTI (ierr)
C      interactive flagging of a single time
C   EDFLTR (ierr)
C      interactive flagging of a time-range
C   EDFRAM (ierr)
C      interactive setting of display frame
C   EDG1DA (fltims, ierr)
C      read in a time-range of data, replacing the selected in-core data
C      All data types are replaced.
C   EDG1SN (ian, fltims, ierr)
C      read in a time-range of SN data, replacing the selected in-core
C      SN data.  All SN data types are replaced.
C   EDG1SY (ian, fltims, ierr)
C      read in a time-range of SY data, replacing the selected in-core
C      SY data.  All SY data types are replaced.
C   EDG1TY (ian, fltims, ierr)
C      read in a time-range of TY data, replacing the selected in-core
C      TY data.  All TY data types are replaced.
C   EDGSIG (type, wt, amp, sig)
C      returns 1 sigma for given type, weight, amp
C   EDGTPT (ant, type, fltims, flflux, ierr)
C      get the next non-flagged sample in the fltims, flflux area
C   EDGTAP (ant, type, fltims, flflux, ierr)
C      get the next sample in the fltims, flflux area
C   EDGTSN (ierr)
C      read in and/or write out 1 or more IFs from SN/CL table
C   EDGTSY (ierr)
C      Gets SY data from OBJECT for specified IF  When editing the SY
C      data themselves it writes the file back again, but not when there
C      is an FC table and UV data are being edited with SY values.
C   EDGTTY (ierr)
C      Gets TY data from object for specified IF  When editing the TY
C      data themselves it writes the file back again, but not when there
C      is an FC table and uv data are being edited with TY values.
C   EDGTUV ( ierr)
C      Gets UV data from OBJECT for specified IF(s), antennas(s)
C   EDIFSY
C      Computes the difference between the current value and a running
C      average value for Pdif and Psum and stores them in locations 3,4
C   EDIFTY
C      Computes the difference between the current value and a running
C      average value for Tsys and stores that in the phase location.
C   EDIFUV
C      Computes the difference between the current value and a running
C      average value for amp and phase
C   EDMAXA (tbedit, antmax, ierr)
C      find max antenna number in a table the hard way
C   EDMXMN (ian, type, pixr, ierr)
C      find max and min of specified data type and antenna/baseline
C   EDMXMT (ian, type, pixr, ierr)
C      if fixed phase plot range, try to force data into range
C   EDOANT (all, ian, lan)
C      determines if the current antenna/baseline is included
C   EDPLOT (ierr)
C      plot the data on the TV
C   EDPLSS (gr, ierr)
C      plot status strings from lower left corner of visible area
C      For SN/CL editing only Now enhanced for TY editing
C   EDPLST (gr, ierr)
C      plot status strings from lower left corner of visible area
C      (calls EDPLSS)
C   EDPLT1 (ian, iprm, corn, gr1, gr2, gr3, it1, it2, dotics, lpixr,
C      ierr)
C      Plots one antenna/baseline worth of data
C   EDPLTP (corn, gr, it, it1, it2, v, sig, lpixr, ierr)
C      plots one point
C   EDRRTR (ierr)
C      interactive re-read of a time range
C   EDRSAR (ierr)
C      interactive restoring of an area: time range and flux range
C   EDRSTI (ierr)
C      interactive restoring (un-flagging) of a single time
C   EDSLST (ierr)
C      fills in the SLIST common variable with a list of source names if
C      there is a source table attached to the object or makes a default
C      list.
C   EDSMTH (ian, type, fltims, ierr)
C      smooth samples in specified range, restore them to plot and
C      toplot, remove them on flagged plot (if any)
C   EDSMTR (ierr)
C      interactive smoothing of a time range
C   EDSORL (fltims, nsor, sorl)
C      returns list of source numbers in a time range
C   EDSTUB (subr, opcode, object)
C      stub point reporter
C   EDTICS (dotics, iprm, corn, gr, pixr, ierr)
C      plot y axis tick marks and labels
C   EDTINC (blc, trc, ntmax, degc, deg, inoi, ticx, itry, ierr)
C      figures out the tick mark lengths and increments for EDIT class
C      plots.  It is limited to vertical axes with linear coordinates.
C   EDTIMI (ot, it)
C      converts extended time range to index renage just interior
C   EDTIMX (it, ot)
C      extends a time range to half way closer to the next time outward
C   EDTWIN (opcode, type, ierr)
C      plots/erases the window marks on the top plot
C   EDVENT (tty, ierr)
C      select points with TV, read new values from user
C   EDWINC (xmin, ymin, ierr)
C      reads current TV parameters, forces TV size to be big enough
C-----------------------------------------------------------------------
C   AP memory allocation:
C      Begin   Number     What
C          1    TIMEM     Times in floating days
C       SUPTR   TIMEM     Source numbers
C       PPTR    TIMEM     Record number of first data sample at time i
C                         as addresses in data area (EDCORE(DPTR++))
C       SPTR    6000+     Scratch area (e.g. 3x2000 for smoothing)
C                         or larger in TYEDT is TIMEM > 6000 for source#
C       DPTR   MAXREC     Data records each DIMREC words long
C                            DIMPRM + Array(1-DIMIF, 1-DIMDAT, 1-POLMAX)
C                            DIMPRM = antenna, time counter, table row#,
C                            DIMIF = EIF-BIF+1  or 1 depending on core
C                            DIMDAT = wt amp phase delay rate -> 3 or 5
C                            POLMAX = 1 or 2
C-----------------------------------------------------------------------
LOCAL INCLUDE 'EDIUTIL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LEDG, CEDG, PINC, SCMAXX, NSRC, NPLT
      PARAMETER (LEDG = 4)
      PARAMETER (CEDG = 5)
      PARAMETER (PINC = 2)
      PARAMETER (SCMAXX = 24000)
      PARAMETER (NSRC = 10000)
      PARAMETER (NPLT = 11)
C
      CHARACTER UVMAST*32, TBEDIT*32, TVNAME*32, DTYPE(8)*12, DDSTR*64,
     *   DDTYPE*2, SMTYPE*4, COLLAB(15)*10, FCFILE*32, SLIST(NSRC)*16,
     *   UVDAT2*32, REASON*24, DUNITS(8)*8
      LOGICAL   DODELY, ALLIN, NEWPLT, ALLPOL, PNDING, UVFLAG,
     *   IFWASN(MAXIF), FLGMSG, DO2ND, FLAGED, ONEANT, FCREAT, APOPEN,
     *   WASACR, WASXCR, ALLTIM, EXPERT, DOCOMP, DOERRB, CROWDP, CROWDI,
     *   ALLSOR, PREXIS, MULTIB, DO3COL, IS3COL, AL3COL
      LONGINT   DPTR, PPTR, SUPTR, SPTR
      INTEGER   BIF, EIF, ANTEN(MAXANT), ANTMAX, POLMAX, IFMAX, IFNOW,
     *   POLNOW, GRSEL(10), ROWMAX, LTVWND(4), LTYPE, LTYPE2, NUMPLT,
     *   PLTAN(NPLT), PIFNOW, EXPLOT(4), XYPLOT(4,NPLT), CSIZE(2),
     *   TIME1, TIME2, TIMEM, ANTMLX, NGRY, NGRPH, MAXX(2), DIMPRM,
     *   DIMIF, DIMDAT, DIMREC, MAXREC, FRQSEL, DDSLEN, WASFLG,
     *   COLNUM(15), SUBARR, SCMAXS, ALLANT, BCHAN, ECHAN, BIF2, EIF2,
     *   BCHAN2, ECHAN2, ANTNOW, PANTNW, TOPLOT, PLTIF, PLTPOL, FLGNMX,
     *   MSAMPS(MAXANT), SUNUMB, ALLIF, FGIFRS(2), CCOLOR, CPLANE
      REAL      TSTART, TEND, DPIXR(2,8), APIXR(2,NPLT), DTIME, GTIME,
     *   RGB8(3), SMTIME(3), DPLSCL(8), APIXR2(2), WTSCAL,
     *   TCAL(4,MAXIF,MAXANT), RSCALE(8), COLORS(3,2*MAXIF), COLOR2(3,2)
      COMMON /EDITCM/ DPTR, PPTR, SUPTR, SPTR, DODELY, BIF, EIF, ANTEN,
     *   ANTMAX, POLMAX, IFMAX, TSTART, TEND, IFNOW, POLNOW, GRSEL,
     *   ROWMAX, LTVWND, LTYPE, NUMPLT, PLTAN, PIFNOW, WASFLG, DPIXR,
     *   EXPLOT, XYPLOT, CSIZE, TIME1, TIME2, TIMEM, ALLIN, APIXR,
     *   ANTMLX, NEWPLT, NGRY, NGRPH, MAXX, DIMPRM, DIMIF, DIMDAT,
     *   DIMREC, MAXREC, DTIME, GTIME, FRQSEL, DDSLEN, ALLANT, ALLPOL,
     *   SMTIME, COLNUM, ALLIF, PNDING, SUBARR, SCMAXS, UVFLAG, IFWASN,
     *   FLGMSG, DO2ND, RGB8, FLAGED, ONEANT, BCHAN, ECHAN, BIF2, EIF2,
     *   BCHAN2, ECHAN2, ANTNOW, PANTNW, FCREAT, APOPEN, WASACR, WASXCR,
     *   ALLTIM, DPLSCL, EXPERT, DOCOMP, DOERRB, LTYPE2, TOPLOT, APIXR2,
     *   WTSCAL, CROWDP, CROWDI, PLTIF, PLTPOL, ALLSOR, FLGNMX, MSAMPS,
     *   SUNUMB, PREXIS, TCAL, RSCALE, FGIFRS, MULTIB, DO3COL, CCOLOR,
     *   CPLANE, COLORS, COLOR2, IS3COL, AL3COL
      COMMON /EDITCH/ UVMAST, TBEDIT, TVNAME, DTYPE, DDSTR, DDTYPE,
     *   SMTYPE, COLLAB, FCFILE, SLIST, UVDAT2, REASON, DUNITS
      INCLUDE 'INCS:DMSG.INC'
LOCAL END
LOCAL INCLUDE 'EDIUTAP.INC'
      LONGINT   EDIPTR
      INTEGER   EDCORI(2), EDSIZE
      REAL      TIMES(2), EDCORE(2)
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (EDCORI(1), EDCORE(1))
      EQUIVALENCE (TIMES(1), EDCORE(1))
      COMMON /EDICOR/ EDIPTR, EDSIZE, EDCORE
LOCAL END
LOCAL INCLUDE 'EDIFCPS.INC'
      INTEGER   FCROW, FLGNUM, FLGANT(2), FLGSOR, FLGCHN(2), FLGIF(2),
     *   FLGSUB, FLGFQ, FLGIT(2)
      CHARACTER FLGOP*8, FLGSTK*4, LDTYPE*8, FLGREA*24
      REAL      FLGTIM(2), DTIMES(2), DFLUXS(2)
LOCAL END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   Public functions:
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE EDITSN (OPCODE, OBJECT, IERR)
C-----------------------------------------------------------------------
C   edit an SN/CL table edit object   --- OR ---
C   edit a UV data object with an SN/CL table object
C   Inputs:
C      OPCODE   C*4    Operation:
C                         'INIT' start a new edit session
C                         'ABOR' close session, delete output table
C                         'KILL' close session, save FC table
C                         'APPL' close session, save output table
C      OBJECT   C*(*)  Open Edit object
C   Output:
C      IERR     I      Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C                         < 0 => dies by the users' hands
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OBJECT*(*)
      INTEGER   IERR
C
      INTEGER   NCOL1, NCOL2, NCOLS, NCOLB, NXPRT, NXPRTB
      PARAMETER (NCOL1 = 22, NCOL2 = 22, NCOLS = NCOL1+NCOL2)
      PARAMETER (NCOLB = 21)
      PARAMETER (NXPRT = 14, NXPRTB = 16)
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'EDIFCPS.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER   TYPE, DIM(7), SCRTCH(256), I, J, K, TVCOL, TVROWS(2),
     *   TIMLIM, CHS, TVWND(4), MINWIN(2), VERS, IG(8), BUTTON, DUMMY,
     *   TTY(2), ITRIM, IVAL(NPLT), NOZOOM(3), SVZOOM(3), ILAST, LT1,
     *   LT2, TVSTAT(16), GRSTAT(8), JERR, IFLN(2), CHSHOW, CHSHO2,
     *   OPTOKX(NXPRT), OPTOKB(NXPRTB), MSGSAV, AVAL(50), ITEMP(2),
     *   NTITLE, SIDSEP
      REAL      T(8), RDUM
      DOUBLE PRECISION DVAL(2), UVFREQ
      LOGICAL   EDAFND, DOPLOT, LOADIT, LEAVE(NCOLS), WANT
      CHARACTER STATUS*4, PROBLM*32, CDUMMY*1, CHOICS(NCOLS)*16,
     *   ISHELP*6, LIST1(NCOL1)*16, LIST2(NCOL2)*16, MSGBUF*72,
     *   DDNAME*18, INKT(4)*8, INEXT*2, LISTB(NCOLB)*16, REAZON*24,
     *   CHOICE*16, CHOICX(NXPRT)*16, SHORTX(NXPRT)*4, ALPHAX(NXPRT)*1,
     *   CHOICB(NXPRTB)*16, SHORTB(NXPRTB)*4, ALPHAB(NXPRTB)*1,
     *   SHOW(4)*16, SHOW2(4)*16, TITLE*8
      DATA INKT /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
      DATA LIST1 /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'RESTORE TIME', 'RESTORE AREA', 'SMOOTH TIME RNG',
     *   'REREAD TIME RNG', 'ENTER SMOOTH PS', 'ENTER AMPL RNG',
     *   'ENTER PHASE RNG', 'ENTER DELAY RNG', 'ENTER RATE RNG',
     *   'USE EXPERT MODE', 'HOLD TV LOAD', 'REPLOT',
     *   ' ', 'EXIT', 'ABORT'/
      DATA LISTB /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'ENTER AMPL RNG', 'ENTER PHASE RNG', 'ENTER DELAY RNG',
     *   'ENTER RATE RNG', 'LIST FLAGS', 'UNDO FLAGS', 'REDO FLAGS',
     *   'SET REASON', 'USE EXPERT MODE', 'HOLD TV LOAD', 'REPLOT',
     *   ' ', 'EXIT', 'ABORT'/
      DATA LIST2 /'NEXT POL/IF', 'SWITCH POLARIZ', 'SWITCH ALL POL',
     *   'ENTER IF', 'SWITCH ALL IF', 'SWITCH ALL TIME',
     *   'SWITCH ALL SOURC', 'SWITCH ALL ANT', 'ENTER ANTENNA',
     *   'ENTER OTHER ANT', 'NEXT ANTENNA', 'PLOT ALL TIMES',
     *   'SELECT FRAME', 'NEXT FRAME', 'PREVIOUS FRAME', 'TV ZOOM',
     *   'OFF ZOOM', 'SHOW AMPLITUDE', 'SHOW DELAY', 'SHOW RATE',
     *   'SHOW ALSO DELAY', 'SHOW ALSO RATE'/
      DATA SHOW, SHOW2 /'SHOW AMPLITUDE',  'SHOW PHASE', 'SHOW DELAY',
     *   'SHOW RATE','SHOW ALSO AMPL', 'SHOW ALSO PHASE',
     *   'SHOW ALSO DELAY', 'SHOW ALSO RATE'/
      DATA CHOICX /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'NEXT POL/IF', 'NEXT ANTENNA', 'SELECT FRAME',
     *   'SHOW AMPLITUDE', 'SHOW PHASE', 'SHOW DELAY', 'SHOW RATE'/
      DATA SHORTX /'Time', 'Rang', 'Belo', 'Abov', 'arEa', 'pOin',
     *   'Quik', 'nCor', 'Nant', 'Fram', 'aMpl', 'Phas', 'Dely', 'Srat'/
      DATA ALPHAX /'T', 'R', 'B', 'A', 'E', 'O', 'Q', 'C', 'N', 'F',
     *   'M', 'P', 'D', 'S'/
      DATA CHOICB /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'LIST FLAGS', 'UNDO FLAGS', 'NEXT POL/IF', 'NEXT ANTENNA',
     *   'SELECT FRAME','SHOW AMPLITUDE', 'SHOW PHASE', 'SHOW DELAY',
     *   'SHOW RATE'/
      DATA SHORTB /'Time', 'Rang', 'Belo', 'Abov', 'arEa', 'pOin',
     *   'Quik', 'List', 'Undo', 'nCor', 'Nant', 'Fram', 'aMpl', 'Phas',
     *   'Dely', 'Srat'/
      DATA ALPHAB /'T', 'R', 'B', 'A', 'E', 'O', 'Q', 'L', 'U', 'C',
     *   'N', 'F', 'M', 'P', 'D', 'S'/
      DATA LEAVE /NCOLS*.TRUE./
C-----------------------------------------------------------------------
      TTY(1) = 5
      TTY(2) = 0
      WASACR = .FALSE.
      WASXCR = .FALSE.
      ALLSOR = .FALSE.
C                                       plot scale ranges
      CALL RFILL (8, 1.0, RSCALE)
      RSCALE(3) = 1.0E-9
      RSCALE(4) = 2.46791709E-13
C                                       Init the functions
      IF (OPCODE.EQ.'INIT') THEN
         FLAGED = .TRUE.
         APOPEN = .FALSE.
         FCREAT = .FALSE.
         DO2ND = .FALSE.
         DOERRB = .FALSE.
         DO 5 I = 1,MAXIF
            IFWASN(I) = .FALSE.
 5          CONTINUE
         FLGMSG = .TRUE.
         SCMAXS = SCMAXX
         FCFILE = ' '
         NEWPLT = .TRUE.
         ILAST = 0
         DTYPE(1) = 'Amplitude'
         DTYPE(2) = 'Phase'
         DTYPE(3) = 'Delay'
         DTYPE(4) = 'Rate'
         DUNITS(1) = 'gains'
         DUNITS(2) = 'Degrees'
         DUNITS(3) = 'nano sec'
         DUNITS(4) = 'milli Hz'
         CALL RFILL (8, 1.0, DPLSCL)
         DPLSCL(3) = 1.E-9
         ALLPOL = .FALSE.
         ALLTIM = .FALSE.
         ALLIF = -1
         SMTYPE = '2PT'
         SMTIME(1) = 300.
         SMTIME(2) = 300.
         SMTIME(3) = 20.
         LOADIT = .TRUE.
         PNDING = .TRUE.
C                                       get attached class names
         PROBLM = OBJECT
         CALL EDIGET (OBJECT, 'UVMASTER', TYPE, DIM, DUMMY, UVMAST,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDIGET (OBJECT, 'TBEDIT', TYPE, DIM, DUMMY, TBEDIT, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDIGET (OBJECT, 'TVDEVICE', TYPE, DIM, DUMMY, TVNAME,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDIGET (OBJECT, 'DOUVFLAG', TYPE, DIM, UVFLAG, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         UVDAT2 = ' '
C                                       get UV data frequency
         PROBLM = UVMAST
         CALL OOPEN (UVMAST, 'READ', IERR)
         IF (IERR.NE.0) GO TO 980
         CALL UVDGET (UVMAST, 'REFFREQ', TYPE, DIM, UVFREQ, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         DPLSCL(4) = 1.E-3 / UVFREQ
         CALL OCLOSE (UVMAST, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Make sure that the table is
C                                       sorted correctly
         CALL TBLSRT (TBEDIT, 'TIME', 'ANTENNA NO.', IERR)
         IF (IERR.NE.0) GO TO 999
         PROBLM = TBEDIT
C                                       Open table object: get adverbs
         STATUS = 'READ'
         CALL TABOPN (TBEDIT, STATUS, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Get a source list
         CALL EDSLST (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       FC table for UV flagging
         IF (UVFLAG) THEN
            SMTYPE = ' '
            FCFILE = 'Flag Command table for SN edits'
            PROBLM = FCFILE
            CALL CREATE (FCFILE, 'TABLE', IERR)
            IF (IERR.NE.0) GO TO 980
            FCREAT = .TRUE.
C                                       copy adverbs to FCFILE
            CALL IN2OBJ (TBEDIT, 4, INKT, INKT, FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Extension type TBLTYPE
            INEXT = 'FC'
            DIM(1) = 2
            DIM(2) = 1
            CALL OPUT (FCFILE, 'TBLTYPE', OOACAR, DIM, DUMMY, INEXT,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       version always 1
            VERS = 1
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (FCFILE, 'VER', OOAINT, DIM, VERS, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       create the FC table file
            CALL OBFEXS (FCFILE, PREXIS, IERR)
            IERR = 0
            IF (PREXIS) THEN
               MSGTXT = '**** WARNING: USING A PRE-EXISTING FC TABLE' //
     *            ' ****'
               CALL MSGWRT (8)
               END IF
            CALL OFCINI (FCFILE, 'WRIT', FLGNUM, FCROW, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *         FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *         FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
            IF (IERR.NE.0) GO TO 980
            FLGNMX = FLGNUM
C                                       edit SN/CL not UV
         ELSE
            FCFILE = ' '
            FLGNMX = 0
            END IF
C                                       get other adverbs
         PROBLM = TBEDIT
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'COMPARE', TYPE, DIM, DOCOMP, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            DOCOMP = .TRUE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'DO3COLOR', TYPE, DIM, RDUM, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            RDUM = -1.0
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'CROWDED', TYPE, DIM, CROWDP, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            CROWDP = .FALSE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         CROWDI = CROWDP
         IF (.NOT.CROWDP) RDUM = -1.0
         DO3COL = RDUM.GT.0.0
         AL3COL = RDUM.GT.1.5
         IS3COL = DO3COL
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'REASON', TYPE, DIM, DUMMY, REASON, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            REASON = ' '
            IERR = 0
            END IF
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'DOEXPERT', TYPE, DIM, EXPERT, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            EXPERT = .FALSE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         IF ((EXPERT) .AND. (.NOT.CROWDP)) THEN
            ALLIF = 1
            ALLPOL = .TRUE.
            END IF
         CALL TABGET (TBEDIT, 'FRQSEL', TYPE, DIM, FRQSEL, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (FRQSEL.EQ.0) FRQSEL = 1
         CALL TABGET (TBEDIT, 'DODELAY', TYPE, DIM, DODELY, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         MSGSUP = 32000
         CALL TABGET (TBEDIT, 'MULTIBAND', TYPE, DIM, MULTIB, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            MULTIB = .FALSE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'VER', TYPE, DIM, VERS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'SUBARR', TYPE, DIM, SUBARR, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         SUBARR = MAX (1, SUBARR)
         CALL TABGET (TBEDIT, 'TIMERANG', TYPE, DIM, T, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         TSTART = T(1) + (T(2) + (T(3) + T(4)/60.) / 60.) / 24.
         TEND = T(5) + (T(6) + (T(7) + T(8)/60.) / 60.) / 24.
         IF (TEND.LE.TSTART) TEND = 999.
         CALL TABGET (TBEDIT, 'EQU_TIME', TYPE, DIM, DTIME, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (DTIME.LE.0.0) DTIME = 1 / 6000.0
         CALL TABGET (TBEDIT, 'GAP_TIME', TYPE, DIM, GTIME, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0 * DTIME)
         CALL TABGET (TBEDIT, 'ANTENNAS', TYPE, DIM, SCRTCH, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
C                                       parameters of SN/CL
         CALL OGET (TBEDIT, 'NROW', TYPE, DIM, ROWMAX, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IFMAX = EIF - BIF + 1
         CALL OGET (TBEDIT, 'KEY.NO_POL', TYPE, DIM, POLMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (POLMAX.LE.0) POLMAX = 1
         IF (POLMAX.GT.2) THEN
             WRITE (MSGTXT,1000) POLMAX
             CALL MSGWRT (6)
             POLMAX = 2
             END IF
         CALL OGET (TBEDIT, 'KEY.NO_ANT', TYPE, DIM, ANTMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (ANTMAX.LE.0) ANTMAX = 1
         CALL FILL (MAXANT, 0, MSAMPS)
         J = 0
         CALL FILL (MAXANT, 0, ANTEN)
         WANT = .TRUE.
         DO 10 I = 1,ANTMAX
            ANTEN(I) = I
            IF (SCRTCH(I).NE.0) J = J + 1
            IF (SCRTCH(I).LT.0) WANT = .FALSE.
 10         CONTINUE
         IF (J.GT.0) THEN
            CALL FILL (MAXANT, 0, ANTEN)
            IF (WANT) THEN
               J = 0
               DO 20 I = 1,ANTMAX
                  IF (SCRTCH(I).GT.0) THEN
                     IF (J.GT.0) THEN
                        DO 15 K = 1,J
                           IF (SCRTCH(I).EQ.ANTEN(K)) GO TO 20
 15                        CONTINUE
                        END IF
                     J = J + 1
                     ANTEN(J) = SCRTCH(I)
                     END IF
 20               CONTINUE
            ELSE
               J = 0
               DO 30 I = 1,ANTMAX
                  DO 25 K = 1,ANTMAX
                     IF (I.EQ.ABS(SCRTCH(K))) GO TO 30
 25               CONTINUE
                  J = J + 1
                  ANTEN(J) = I
 30               CONTINUE
               END IF
            ANTMLX = J
         ELSE
            ANTMLX = ANTMAX
            END IF
C                                       build descriptor string
         CALL TABGET (TBEDIT, 'NAME', TYPE, DIM, DUMMY, DDNAME(:12),
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'CLASS', TYPE, DIM, DUMMY, DDNAME(13:),
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'IMSEQ', TYPE, DIM, J, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL NAMEST (DDNAME, J, DDSTR, DDSLEN)
         CALL TABGET (TBEDIT, 'TBLTYPE', TYPE, DIM, DUMMY, DDTYPE, IERR)
         IF (IERR.NE.0) GO TO 980
         IF ((DDTYPE.NE.'SN') .AND. (DDTYPE.NE.'CL')) MULTIB = .FALSE.
         IF (MULTIB) THEN
            BIF = 1
            EIF = 1
            DOCOMP = .FALSE.
            CROWDP = .FALSE.
            DTYPE(1) = 'MB delay'
            DUNITS(1) = 'nano sec'
            DPLSCL(1) = 1.E-9
            RSCALE(1) = 1.0E-9
            ALLIF = 1
            END IF
         DDSLEN = DDSLEN + 4
         DDSTR(DDSLEN:) = DDTYPE // ' VERS'
         DDSLEN = DDSLEN + 8
         WRITE (MSGBUF,1030) VERS
         CALL CHTRIM (MSGBUF, 8, MSGBUF, J)
         DDSTR(DDSLEN:) = MSGBUF(:J)
         DDSLEN = DDSLEN + 3 + J
         DDSTR(DDSLEN:) = 'FQID'
         DDSLEN = DDSLEN + 5
         WRITE (MSGBUF,1030) FRQSEL
         CALL CHTRIM (MSGBUF, 8, MSGBUF, J)
         DDSTR(DDSLEN:) = MSGBUF(:J)
         DDSLEN = DDSLEN + J - 1
C                                       close and reopen for update
         CALL TABCLO (TBEDIT, IERR)
         IF (IERR.NE.0) GO TO 980
         STATUS = 'WRIT'
         CALL TABOPN (TBEDIT, STATUS, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       basic parameters
         TIMEM = 0
         TIME1 = 1
         TIME2 = 0
         ALLIN = .FALSE.
         PIFNOW = -1
         MAXREC = 0
         POLNOW = 1
         IF (CROWDP) POLNOW = 0
         LTYPE = 2
         IF (MULTIB) LTYPE = 1
         LTYPE2 = 0
         IF (DOCOMP) LTYPE2 = 1
         IFNOW = BIF
         IF (CROWDI) IFNOW = 0
C                                       set default ranges to full range
C                                       of phases, -1000:1000 ns & mHz
         IF (EXPERT) THEN
            DPIXR(1,1) = 0.0
            DPIXR(2,1) = -1.0
            DPIXR(1,2) = -180.0 * DPLSCL(2)
            DPIXR(2,2) = 180.0 * DPLSCL(2)
            DPIXR(1,3) = -1000.0 * DPLSCL(3)
            DPIXR(2,3) = 1000.0 * DPLSCL(3)
            DPIXR(1,4) = -1000.0 * DPLSCL(4)
            DPIXR(2,4) = 1000.0 * DPLSCL(4)
C                                       self scale in non-expert
         ELSE
            CALL RFILL (8, 0.0, DPIXR)
            END IF
C                                       Open terminal for conversation
         CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *      IERR)
         IF (IERR.NE.0) THEN
            TTY(2) = 0
            WRITE (MSGTXT,1035) IERR
            CALL MSGWRT (8)
            PROBLM = 'The terminal'
            GO TO 980
            END IF
         TTY(2) = MAX (1, TTY(2))
C                                       Graphics: menu, menu back,
C                                       editing, editdata, flagged data,
C                                       extra data
C                                       2 3 4 1 6 5 7
         GRSEL(1) = 6
         GRSEL(2) = 3
         GRSEL(3) = 4
         GRSEL(4) = 1
         GRSEL(5) = 5
         GRSEL(6) = 2
         GRSEL(7) = 0
C                                       learn about TV
         PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         MINWIN(1) = (36 + CEDG) * CSIZE(1) + 300
         MINWIN(2) = 37 * CSIZE(2) + 150
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
C                                       off all TV channels
         CALL TVDOPR (TVNAME, 'HOLD', I, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDOPR (TVNAME, 'INIT', I, IERR)
         IF (IERR.NE.0) GO TO 980
         I = 1
         CALL TVDOPR (TVNAME, 'TVOF', I, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       clear and off all graphics
         DO 35 I = 1,NGRPH-1
            CALL TVDOPR (TVNAME, 'GRON', I, IERR)
            IF (IERR.NE.0) GO TO 980
 35         CONTINUE
         CALL TVDFUN (TVNAME, 'OFFZ', 0, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDZOM (TVNAME, 'READ', NOZOOM, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (DO3COL) THEN
            CALL ED3CIN (IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, IERR)
         IF (IERR.NE.0) GO TO 980

C                                       Get some data
         WASFLG = -1
         PROBLM = TBEDIT
         CALL EDGTSN (IERR)
         IF (IERR.NE.0) GO TO 980
         APOPEN = .TRUE.
         IF (POLMAX.LE.1) THEN
            CROWDP = .FALSE.
            POLNOW = 1
            END IF
         IF (.NOT.ALLIN) THEN
            CROWDI = .FALSE.
            IFNOW = BIF
            END IF
C                                       get initial antennas to plot
         NUMPLT = 0
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'ANTS2USE', TYPE, DIM, AVAL, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            CALL FILL (50, 0, AVAL)
            IERR = 0
            END IF
         DO 40 I = 1,NPLT
            IF ((AVAL(I).GT.0) .AND. (AVAL(I).LE.ANTMAX) .AND.
     *         (EDAFND(AVAL(I),ANTMLX,ANTEN)) .AND.
     *         (MSAMPS(AVAL(I)).GT.0)) THEN
               NUMPLT = NUMPLT + 1
               PLTAN(NUMPLT) = AVAL(I)
               END IF
 40         CONTINUE
         IF (NUMPLT.LE.0) THEN
            DO 45 I = 1,ANTMLX
               IF (MSAMPS(ANTEN(I)).GT.0) THEN
                  NUMPLT = NUMPLT + 1
                  IF (NUMPLT.LE.NPLT) PLTAN(NUMPLT) = ANTEN(I)
                  END IF
 45            CONTINUE
            NUMPLT = MIN (NUMPLT, 3)
            END IF
         ALLANT = PLTAN(1)
C                                       display the data
         PROBLM = TVNAME
         CALL EDPLOT (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       set parameters for menu
         TVCOL = 2
         IF (UVFLAG) THEN
            TVROWS(1) = NCOLB - 2
            IF (DODELY) TVROWS(1) = NCOLB
            IF (MULTIB) TVROWS(1) = NCOLB - 3
            DO 50 I = 1,NCOLB
               CHOICS(I) = LISTB(I)
 50            CONTINUE
            IF (MULTIB) CHOICS(8) = 'ENTER MBDL RNG'
            IF ((.NOT.DODELY) .OR. (MULTIB)) THEN
               DO 52 I = 1,10
                  CHOICS(TVROWS(1)-I+1) = LISTB(NCOLB-I+1)
 52               CONTINUE
               END IF
         ELSE
            TVROWS(1) = NCOL1 - 2
            IF (DODELY) TVROWS(1) = NCOL1
            IF (MULTIB) TVROWS(1) = NCOL1 - 3
            DO 55 I = 1,NCOL1
               CHOICS(I) = LIST1(I)
 55            CONTINUE
            IF (MULTIB) CHOICS(13) = 'ENTER MBDL RNG'
            IF ((.NOT.DODELY) .OR. (MULTIB)) THEN
               DO 56 I = 1,6
                  CHOICS(TVROWS(1)-I+1) = LIST1(NCOL1-I+1)
 56               CONTINUE
               END IF
            END IF
         J = TVROWS(1)
         IF (POLMAX*(EIF-BIF+1).GT.2) THEN
            J = J + 1
            CHOICS(J) = LIST2(1)
            END IF
         IF (POLMAX.GT.1) THEN
            J = J + 1
            CHOICS(J) = LIST2(2)
            J = J + 1
            CHOICS(J) = LIST2(3)
            END IF
         IF (EIF.GT.BIF) THEN
            J = J + 1
            CHOICS(J) = LIST2(4)
            IF (DIMIF.GT.1) THEN
               J = J + 1
               CHOICS(J) = LIST2(5)
               END IF
            END IF
         CHSHO2 = 0
         DO 60 I = 6,NCOL2
            IF (MULTIB) THEN
               IF (I.LT.NCOL2-5) THEN
                  J = J + 1
                  CHOICS(J) = LIST2(I)
                  END IF
            ELSE IF ((I.LT.NCOL2-3) .OR. (DODELY)) THEN
               J = J + 1
               CHOICS(J) = LIST2(I)
               IF (CHOICS(J).EQ.'SHOW AMPLITUDE') CHSHOW = J
               IF (CHOICS(J)(:10).EQ.'SHOW ALSO ') THEN
                  IF (CHSHO2.LE.0) CHSHO2 = J
                  IF (.NOT.DOCOMP) J = J - 1
                  END IF
               END IF
 60         CONTINUE
         TVROWS(2) = J - TVROWS(1)
         IF (UVFLAG) THEN
            ISHELP = 'EDICL'
         ELSE
            ISHELP = 'EDISN'
            END IF
         TIMLIM = 0
         IG(1) = GRSEL(1)
         IG(2) = GRSEL(2)
         CALL FILL (NXPRT, 1, OPTOKX)
         CALL FILL (NXPRTB, 1, OPTOKB)
         IF ((.NOT.DODELY) .OR. (MULTIB)) THEN
            OPTOKX(NXPRT) = -1
            OPTOKX(NXPRT-1) = -1
            OPTOKB(NXPRTB) = -1
            OPTOKB(NXPRTB-1) = -1
            END IF
         OPTOKX(NXPRT-2) = -1
         OPTOKB(NXPRTB-2) = -1
         IF (MULTIB) THEN
            OPTOKX(NXPRT-3) = -1
            OPTOKB(NXPRTB-3) = -1
            END IF
         IF ((POLMAX.LE.1) .AND. (EIF.EQ.BIF)) THEN
            OPTOKX(NXPRT-6) = -1
            OPTOKB(NXPRTB-6) = -1
            END IF

C                                       LOOP POINT
C                                       window still big enough?
 100     PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
         I = LTVWND(4) - TOPLOT + 5 * CSIZE(2) - 1
         IF (NEWPLT) THEN
            CALL TVDOKA (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            GRSTAT(IG(1)) = 1
            CALL TVDRST (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            ILAST = I
            END IF
         IF (PREXIS) THEN
            CHOICE = 'REDO FLAGS'
         ELSE IF ((EXPERT) .AND. (UVFLAG)) THEN
            CALL TVDXPR (TVNAME, -1, TVCOL, TVROWS, IG, I, CHOICS,
     *         LEAVE, TTY, NXPRTB, CHOICB, SHORTB, ALPHAB, OPTOKB,
     *         CHOICE, IERR)
            PROBLM = 'The terminal'
            IF (IERR.NE.0) GO TO 980
            J = TVROWS(1) + TVROWS(2)
            CALL EDCFND (CHOICE, J, CHOICS, CHS)
         ELSE IF (EXPERT) THEN
            CALL TVDXPR (TVNAME, -1, TVCOL, TVROWS, IG, I, CHOICS,
     *         LEAVE, TTY, NXPRT, CHOICX, SHORTX, ALPHAX, OPTOKX,
     *         CHOICE, IERR)
            PROBLM = 'The terminal'
            IF (IERR.NE.0) GO TO 980
            J = TVROWS(1) + TVROWS(2)
            CALL EDCFND (CHOICE, J, CHOICS, CHS)
         ELSE
            TITLE = ' '
            NTITLE = 0
            SIDSEP = 7
            CALL TVDMEN (TVNAME, -1, TVCOL, TVROWS, IG, I, SIDSEP,
     *         ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHS,
     *         BUTTON, IERR)
            IF (IERR.NE.0) GO TO 980
            CHOICE = CHOICS(CHS)
            END IF
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
         IF (NEWPLT) ILAST = 0
C                                       Do something:
         IF (LOADIT) DOPLOT = .FALSE.
C                                       load/hold
         IF ((CHOICE.EQ.'HOLD TV LOAD') .OR.
     *      (CHOICE.EQ.'DO TV LOAD')) THEN
            IF (LOADIT) THEN
               CHOICS(CHS) = 'DO TV LOAD'
            ELSE
               CHOICS(CHS) = 'HOLD TV LOAD'
               END IF
            ILAST = 0
            LOADIT = .NOT.LOADIT
C                                       flag interaction
         ELSE IF (CHOICE(:5).EQ.'FLAG ') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (CHOICE.EQ.'FLAG TIME') THEN
               CALL EDFLTI (IERR)
            ELSE IF (CHOICE.EQ.'FLAG TIME RANGE') THEN
               CALL EDFLTR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG BELOW') THEN
               CALL EDFLFB (IERR)
            ELSE IF (CHOICE.EQ.'FLAG ABOVE') THEN
               CALL EDFLFA (IERR)
            ELSE IF (CHOICE.EQ.'FLAG AREA') THEN
               CALL EDFLAR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG POINT') THEN
               CALL EDFLPT (IERR)
            ELSE IF (CHOICE.EQ.'FLAG QUICKLY') THEN
               CALL EDFLQU (IERR)
            ELSE
               CALL EDSTUB ('FLAG', CHOICE, OBJECT)
               END IF
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       list FC table
         ELSE IF (CHOICE.EQ.'LIST FLAGS') THEN
            CALL EDFCLI (IERR)
C                                       Undo flags in FC table
         ELSE IF (CHOICE.EQ.'UNDO FLAGS') THEN
            IF (FLGNMX.LE.1) THEN
               IFLN(1) = FLGNMX
               IFLN(2) = FLGNMX
               IERR = 0
            ELSE
               WRITE (MSGBUF,1100) FLGNMX
               CALL INQINT (TTY, MSGBUF, 2, IFLN, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
               IF (IFLN(2).LT.IFLN(1)) IFLN(2) = IFLN(1)
               END IF
            IF ((IERR.EQ.0) .AND. (IFLN(1).GT.0)) THEN
               PROBLM = TVNAME
               CALL EDFCUN (IFLN, IERR)
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Redo flags in FC table
         ELSE IF (CHOICE.EQ.'REDO FLAGS') THEN
            CALL EDFCDO (IERR)
            PREXIS = .FALSE.
C                                       interp at flag point
         ELSE IF (CHOICE.EQ.'SMOOTH TIME RNG') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL EDSMTR (IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Re-read at flag point
         ELSE IF (CHOICE.EQ.'REREAD TIME RNG') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL EDRRTR (IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Unflag
         ELSE IF (CHOICE(:8).EQ.'RESTORE ') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (CHOICE.EQ.'RESTORE TIME') THEN
               CALL EDRSTI (IERR)
            ELSE IF (CHOICE.EQ.'RESTORE AREA') THEN
               CALL EDRSAR (IERR)
            ELSE
               CALL EDSTUB ('RESTORE', CHOICE, OBJECT)
               END IF
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'ENTER ANTENNA') THEN
            MSGBUF = 'Enter number of antenna to be edited'
            CALL INQINT (TTY, MSGBUF, 1, I, IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.LT.0) THEN
               I = PLTAN(1)
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
            IF ((I.LE.0) .OR. (I.GT.ANTMAX)) THEN
               WRITE (MSGTXT,1210) 'ANTENNA', I, 1, ANTMAX
               CALL MSGWRT (6)
            ELSE IF ((EDAFND (I, ANTMLX, ANTEN)) .AND. (MSAMPS(I).GT.0))
     *         THEN
               IF (I.NE.PLTAN(1)) THEN
                  LT1 = 0
                  LT2 = 0
                  DO 105 J = 2,NUMPLT
                     IF (PLTAN(J).EQ.I) LT1 = J
                     IF (PLTAN(J).EQ.PLTAN(1)) LT2 = J
 105                 CONTINUE
                  IF (LT1.GT.0) THEN
                     IF (LT2.EQ.0) THEN
                        PLTAN(LT1) = PLTAN(1)
                     ELSE
                        CALL COPY (NUMPLT-LT1, PLTAN(LT1+1), PLTAN(LT1))
                        NUMPLT = NUMPLT - 1
                        END IF
                     END IF
                  PLTAN(1) = I
                  IF (ALLANT.GT.0) ALLANT = PLTAN(1)
                  DOPLOT = .TRUE.
                  PNDING = .TRUE.
                  END IF
            ELSE
               WRITE (MSGTXT,1211) I
               CALL MSGWRT (6)
               END IF
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'NEXT ANTENNA') THEN
            I = PLTAN(1)
 110        I = I + 1
               IF (I.GT.ANTMAX) I = 1
               IF (MSAMPS(I).LE.0) GO TO 110
               IF (.NOT.(EDAFND (I, ANTMLX, ANTEN))) GO TO 110
            IF (I.NE.PLTAN(1)) THEN
               LT1 = 0
               LT2 = 1
               DO 115 J = 2,NUMPLT
 111              IF (PLTAN(J).EQ.I) THEN
                     CALL COPY (NUMPLT-J, PLTAN(J+1), PLTAN(J))
                     PLTAN(NUMPLT) = 0
                     GO TO 111
                     END IF
                  LT1 = MAX (LT1, PLTAN(J))
                  IF (PLTAN(J).GT.0) LT2 = J
 115              CONTINUE
               PLTAN(1) = I
               IF (ALLANT.GT.0) ALLANT = PLTAN(1)
C                                       add to other ants
 120           IF (LT2.LT.NUMPLT) THEN
                  I = LT1
                  IF (I.EQ.0) I = PLTAN(1)
                  DO 125 K = 1,ANTMAX
                     I = I + 1
                     IF (I.GT.ANTMAX) I = 1
C                                       good antenna - used already?
                     IF ((EDAFND (I, ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(I).GT.0)) THEN
                        DO 121 J = 1,LT2
                           IF (I.EQ.PLTAN(J)) GO TO 125
 121                       CONTINUE
C                                       found a new one
                        LT2 = LT2 + 1
                        PLTAN(LT2) = I
                        LT1 = MAX (LT1, I)
                        GO TO 120
                        END IF
 125                 CONTINUE
                  NUMPLT = LT2
                  END IF
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Other ants
         ELSE IF (CHOICE.EQ.'ENTER OTHER ANT') THEN
            I = NPLT - 1
            WRITE (MSGBUF,1220) I
            CALL INQINT (TTY, MSGBUF, I, IVAL(2), IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.EQ.0) THEN
               K = NUMPLT
               NUMPLT = 1
               DO 210 I = 2,NPLT
                  IF ((IVAL(I).GT.0) .AND. (IVAL(I).LE.ANTMAX)) THEN
                     IF ((EDAFND (IVAL(I), ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(IVAL(I)).GT.0)) THEN
                        NUMPLT = NUMPLT + 1
                        PLTAN(NUMPLT) = IVAL(I)
                        END IF
                     END IF
 210              CONTINUE
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               IF (K.NE.NUMPLT) THEN
                  CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
                  IF (IERR.NE.0) GO TO 980
                  NEWPLT = .TRUE.
                  ILAST = 0
                  END IF
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Select IF
         ELSE IF (CHOICE.EQ.'ENTER IF') THEN
            IF ((EIF.EQ.BIF+1) .AND. (.NOT.CROWDI)) THEN
               I = 2 * BIF + 1 - IFNOW
               IERR = 0
            ELSE IF ((EIF.EQ.BIF+1) .AND. (CROWDI)) THEN
               IF (IFNOW.EQ.0) THEN
                  I = BIF
               ELSE IF (IFNOW.EQ.EIF) THEN
                  I = 0
               ELSE
                  I = EIF
                  END IF
               IERR = 0
            ELSE
               MSGBUF = 'Enter number of IF to be plotted and edited'
               CALL INQINT (TTY, MSGBUF, 1, I, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
               IF (IERR.LT.0) THEN
                  I = IFNOW
                  MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                  CALL MSGWRT (6)
                  IERR = 0
                  END IF
               END IF
            IF (((I.LT.BIF) .OR. (I.GT.EIF)) .AND. ((I.NE.0) .OR.
     *         (.NOT.CROWDI))) THEN
               WRITE (MSGTXT,1210) 'IF', I, BIF, EIF
               CALL MSGWRT (6)
            ELSE IF (I.NE.IFNOW) THEN
               PNDING = .TRUE.
               PIFNOW = IFNOW
               IFNOW = I
               IF ((ALLIF.EQ.0) .AND. ((IFNOW.LT.FGIFRS(1)) .OR.
     *            (IFNOW.GT.FGIFRS(2)))) THEN
                  WRITE (MSGTXT,1221) IFNOW, FGIFRS
                  CALL MSGWRT (7)
                  END IF
               PROBLM = TBEDIT
               IF (.NOT.ALLIN) THEN
                  CALL EDGTSN (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               DOPLOT = .TRUE.
               END IF
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Smooth parameters
         ELSE IF (CHOICE.EQ.'ENTER SMOOTH PS') THEN
            MSGBUF = 'Enter smooth type (2PT BOX MWF 2PTH GAUS LINE '
     *         // 'EXP) left justified'
 220        CALL INQSTR (TTY, MSGBUF, 4, STATUS, IERR)
            IF (IERR.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 220
               END IF
            CALL CHLTOU (4, STATUS)
            IF ((IERR.EQ.0) .AND. ((STATUS.EQ.'2PT') .OR.
     *         (STATUS.EQ.'BOX') .OR. (STATUS.EQ.'MWF') .OR.
     *         (STATUS.EQ.'2PTH') .OR. (STATUS.EQ.'GAUS') .OR.
     *         (STATUS.EQ.'LINE') .OR. (STATUS.EQ.'EXP '))) THEN
               SMTYPE = STATUS
               MSGBUF = 'Enter smooth box full width in seconds'
               CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
               IF ((IERR.EQ.0) .AND. (I.GT.0) .AND. (I.LT.86400)) THEN
                  SMTIME(2) = DVAL(1)
                  IF ((SMTYPE.EQ.'GAUS') .OR. (SMTYPE.EQ.'LINE') .OR.
     *               (SMTYPE.EQ.'EXP ')) THEN
                     MSGBUF = 'Enter full width half max in seconds'
                     CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
                     IF ((IERR.EQ.0) .AND. (I.GT.0) .AND. (I.LT.86400))
     *                  THEN
                        SMTIME(3) = DVAL(1)
                        CALL EDPLST (GRSEL(6), IERR)
                     ELSE
                        MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                        CALL MSGWRT (6)
                        END IF
                     END IF
               ELSE
                  MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                  CALL MSGWRT (6)
                  END IF
            ELSE IF ((IERR.NE.0) .OR. (STATUS.NE.' ')) THEN
               MSGTXT = 'THERE WAS A PROBLEM: SMTYPE KEPT AS ' // SMTYPE
               CALL MSGWRT (6)
               IERR = 0
               END IF
            IERR = MAX (0, IERR)
C                                       Set reason
         ELSE IF (CHOICE.EQ.'SET REASON') THEN
            MSGBUF = 'Enter new reason, left justified'
 221        CALL INQSTR (TTY, MSGBUF, 24, REAZON, IERR)
            IF (IERR.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 221
               END IF
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF ((IERR.EQ.0) .AND. (REAZON.NE.'-')) THEN
               REASON = REAZON
               I = ITRIM (REASON)
               I = MAX (1, I)
               MSGTXT = 'Using reason = ''' // REASON(:I) // ''''
               CALL MSGWRT (2)
               END IF
            IERR = MAX (0, IERR)
C                                       Set ranges
         ELSE IF (CHOICE(:6).EQ.'ENTER ') THEN
            I = 0
            IF (CHOICE.EQ.'ENTER AMPL RNG') I = 1
            IF (CHOICE.EQ.'ENTER PHASE RNG') I = 2
            IF (CHOICE.EQ.'ENTER DELAY RNG') I = 3
            IF (CHOICE.EQ.'ENTER RATE RNG') I = 4
            IF (CHOICE.EQ.'ENTER MBDL RNG') I = 1
            IF (I.GT.0) THEN
               J = ITRIM (DTYPE(I))
               MSGBUF = 'Enter ' // DTYPE(I)(:J) // ' display range in '
     *            // DUNITS(I)
               CALL INQFLT (TTY, MSGBUF, 2, DVAL, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
               IF (IERR.EQ.0) THEN
                  DPIXR(1,I) = DVAL(1) * DPLSCL(I)
                  DPIXR(2,I) = DVAL(2) * DPLSCL(I)
                  IF ((LTYPE.EQ.I) .OR. ((DOCOMP) .AND. (LTYPE2.EQ.I)))
     *               DOPLOT = .TRUE.
                  PNDING = .TRUE.
               ELSE
                  MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                  CALL MSGWRT (6)
                  IERR = 0
                  END IF
            ELSE
               CALL EDSTUB ('ENTER', CHOICE, OBJECT)
               END IF
C                                       Switch polarization
         ELSE IF (CHOICE.EQ.'SWITCH POLARIZ') THEN
            IF (CROWDP) THEN
               POLNOW = MOD (POLNOW+1, 3)
            ELSE
               POLNOW = 3 - POLNOW
               END IF
            DOPLOT = .TRUE.
            PNDING = .TRUE.
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Next Correlator
         ELSE IF (CHOICE.EQ.'NEXT POL/IF') THEN
            PNDING = .TRUE.
            IF (POLNOW.LT.POLMAX) THEN
               POLNOW = POLNOW + 1
               IFNOW = MAX (BIF, IFNOW)
            ELSE
               POLNOW = 1
               IF (CROWDP) POLNOW = 0
               PIFNOW = IFNOW
               IFNOW = MAX (BIF, IFNOW+1)
               IF (IFNOW.GT.EIF) THEN
                  IFNOW = BIF
                  IF (CROWDI) IFNOW = 0
                  END IF
               PROBLM = TBEDIT
               IF (.NOT.ALLIN) THEN
                  CALL EDGTSN (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               END IF
            DOPLOT = .TRUE.
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Switch all ant
         ELSE IF (CHOICE.EQ.'SWITCH ALL ANT') THEN
            IF (ALLANT.GT.0) THEN
               ALLANT = 0
            ELSE
               ALLANT = PLTAN(1)
               END IF
            CALL EDPLST (GRSEL(6), IERR)
C                                       Switch all pol
         ELSE IF (CHOICE.EQ.'SWITCH ALL POL') THEN
            IF (POLMAX.GT.1) THEN
               ALLPOL = .NOT.ALLPOL
               CALL EDPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all IF
         ELSE IF (CHOICE.EQ.'SWITCH ALL IF') THEN
            IF (DIMIF.GT.1) THEN
               IF ((DIMIF.GT.2) .AND. (ALLIN)) THEN
                  ALLIF = ALLIF + 1
                  IF (ALLIF.EQ.2) THEN
                     ALLIF = -1
                  ELSE IF (ALLIF.EQ.0) THEN
                     MSGBUF = 'Enter IF number range to flag'
                     CALL INQINT (TTY, MSGBUF, 2, ITEMP, IERR)
                     PROBLM = 'The terminal'
                     IF (IERR.GT.0) GO TO 980
                     IF (IERR.EQ.0) THEN
                        IF ((ITEMP(1).EQ.0) .AND. (ITEMP(2).EQ.0)) THEN
                           ALLIF = -1
                        ELSE IF ((ITEMP(2).LT.ITEMP(1)) .OR.
     *                     (ITEMP(1).LT.BIF) .OR. (ITEMP(1).GT.EIF) .OR.
     *                     (ITEMP(2).LT.BIF) .OR. (ITEMP(2).GT.EIF))
     *                     THEN
                           IERR = -1
                        ELSE
                           FGIFRS(1) = ITEMP(1)
                           FGIFRS(2) = ITEMP(2)
                           IF ((IFNOW.LT.FGIFRS(1)) .OR.
     *                        (IFNOW.GT.FGIFRS(2))) THEN
                              WRITE (MSGTXT,1221) IFNOW, FGIFRS
                              CALL MSGWRT (7)
                              END IF
                           END IF
                        END IF
                     IF (IERR.LT.0) THEN
                        I = IFNOW
                        MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                        CALL MSGWRT (6)
                        IERR = 0
                        END IF
                     END IF
               ELSE
                  ALLIF = -ALLIF
                  END IF
               CALL EDPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all pol
         ELSE IF (CHOICE.EQ.'SWITCH ALL TIME') THEN
            ALLTIM = .NOT.ALLTIM
            CALL EDPLST (GRSEL(6), IERR)
C                                       Switch all source
         ELSE IF (CHOICE.EQ.'SWITCH ALL SOURC') THEN
            ALLSOR = .NOT.ALLSOR
            CALL EDPLST (GRSEL(6), IERR)
C                                       TV zoom
         ELSE IF (CHOICE.EQ.'TV ZOOM') THEN
            CALL OTVZOM (TVNAME, IERR)
            CALL TVDZOM (TVNAME, 'READ', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Off zoom
         ELSE IF (CHOICE.EQ.'OFF ZOOM') THEN
            CALL OTVOFZ (TVNAME, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'READ', SVZOOM, IERR)
C                                       expert mode switch
         ELSE IF (CHOICE.EQ.'USE EXPERT MODE') THEN
            EXPERT = .NOT.EXPERT
C                                       full plot
         ELSE IF (CHOICE.EQ.'PLOT ALL TIMES') THEN
            IF ((TIME1.GT.1) .OR. (TIME2.LT.TIMEM)) THEN
               TIME1 = 1
               TIME2 = TIMEM-1
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Select frame
         ELSE IF (CHOICE.EQ.'SELECT FRAME') THEN
            IF ((TIME1.GT.1) .OR. (TIME2.LT.TIMEM-1) .OR. PNDING) THEN
               TIME1 = 1
               TIME2 = TIMEM-1
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL EDFRAM (IERR)
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Next frame
         ELSE IF (CHOICE.EQ.'NEXT FRAME') THEN
            LT2 = MIN (2*TIME2-TIME1, TIMEM-1)
            LT1 = LT2 - TIME2 + TIME1
            IF ((TIME1.NE.LT1) .OR. (TIME2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               TIME1 = LT1
               TIME2 = LT2
               END IF
C                                       Previous frame
         ELSE IF (CHOICE.EQ.'PREVIOUS FRAME') THEN
            LT1 = MAX (2*TIME1-TIME2, 1)
            LT2 = LT1 + TIME2 - TIME1
            IF ((TIME1.NE.LT1) .OR. (TIME2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               TIME1 = LT1
               TIME2 = LT2
               END IF
C                                       Show also type
         ELSE IF (CHOICE(:10).EQ.'SHOW ALSO ') THEN
C                                       gets here only if dodely true
            IF ((DOCOMP) .AND. (DODELY)) THEN
               J = LTYPE2
               IF (CHOICE(11:).EQ.'AMPL') LTYPE2 = 1
               IF (CHOICE(11:).EQ.'PHASE') LTYPE2 = 2
               IF (CHOICE(11:).EQ.'DELAY') LTYPE2 = 3
               IF (CHOICE(11:).EQ.'RATE') LTYPE2 = 4
               K = CHSHO2
               DO 225 I = 1,4
                  IF ((I.NE.LTYPE) .AND. (I.NE.LTYPE2)) THEN
                     CHOICS(K) = SHOW2(I)
                     K = K + 1
                     END IF
 225              CONTINUE
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       gets here only if dodely true
C                                       Show a type
         ELSE IF (CHOICE(:5).EQ.'SHOW ') THEN
            I = LTYPE
            J = LTYPE2
            IF (CHOICE.EQ.'SHOW AMPLITUDE') LTYPE = 1
            IF (CHOICE.EQ.'SHOW PHASE') LTYPE = 2
            IF (CHOICE.EQ.'SHOW DELAY') LTYPE = 3
            IF (CHOICE.EQ.'SHOW RATE') LTYPE = 4
            IF (DOCOMP) THEN
               IF ((DODELY) .AND. (LTYPE2.EQ.LTYPE)) THEN
                  IF (LTYPE.GT.2) LTYPE2 = 7 - LTYPE
                  IF (LTYPE.LE.2) LTYPE2 = 3 - LTYPE
               ELSE IF (.NOT.DODELY) THEN
                  LTYPE2 = 3 - LTYPE
                  END IF
               END IF
            IF ((I.NE.LTYPE) .OR. (J.NE.LTYPE2)) THEN
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               IF (UVFLAG) THEN
                  OPTOKB(NXPRTB-4+I) = 1
                  OPTOKB(NXPRTB-4+LTYPE) = -1
               ELSE
                  OPTOKX(NXPRT-4+I) = 1
                  OPTOKX(NXPRT-4+LTYPE) = -1
                  END IF
               IF (DODELY) THEN
                  K = CHSHOW
                  DO 230 I = 1,4
                     IF (I.NE.LTYPE) THEN
                        CHOICS(K) = SHOW(I)
                        K = K + 1
                        END IF
 230                 CONTINUE
                  IF (DOCOMP) THEN
                     K = CHSHO2
                     DO 235 I = 1,4
                        IF ((I.NE.LTYPE) .AND. (I.NE.LTYPE2)) THEN
                           CHOICS(K) = SHOW2(I)
                           K = K + 1
                           END IF
 235                    CONTINUE
                     END IF
               ELSE
                  CHOICS(CHSHOW) = SHOW(3-LTYPE)
                  END IF
               END IF
C                                       REPLOT
         ELSE IF (CHOICE.EQ.'REPLOT') THEN
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       EXIT
         ELSE IF (CHOICE.EQ.'EXIT') THEN
            IERR = 0
            GO TO 995
C                                       ABORT
         ELSE IF (CHOICE.EQ.'ABORT') THEN
            IERR = -1
            GO TO 995
            END IF
         IF ((IERR.EQ.0) .AND. (DOPLOT)) THEN
            IF (LOADIT) THEN
               CALL EDPLOT (IERR)
            ELSE IF (PNDING) THEN
               CALL EDPLST (GRSEL(6), IERR)
               END IF
            PROBLM = TVNAME
            END IF
         IF (IERR.NE.0) GO TO 980
         GO TO 100
C                                       apply result
      ELSE IF (OPCODE.EQ.'APPL') THEN
         PROBLM = TBEDIT
         PIFNOW = IFNOW
         IFNOW = -1
         CALL EDGTSN (IERR)
         IF (IERR.NE.0) GO TO 980
         IF (UVFLAG) THEN
            CALL EDFCAP (UVMAST, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TABZAP (FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'EDGTSN', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 800 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 800        CONTINUE
         IF (DO3COL) THEN
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+1, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+2, IERR)
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       destroy result
      ELSE IF ((OPCODE.EQ.'ABOR') .OR. (OPCODE.EQ.'KILL')) THEN
         IF ((UVFLAG) .AND. (FCREAT) .AND. (OPCODE.NE.'KILL')) THEN
            PROBLM = FCFILE
            CALL TABZAP (FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'EDGTSN', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 810 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 810        CONTINUE
         IF (DO3COL) THEN
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+1, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+2, IERR)
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       ??
      ELSE
         IERR = 2
         CALL EDSTUB ('EDITSN', OPCODE, OBJECT)
         END IF
      GO TO 995
C                                       error
 980  MSGTXT = 'EDITSN: ERROR WITH OBJECT ' // PROBLM
      CALL MSGWRT (7)
C
 995  IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NO_POL = ',I3,' LIMITED HERE TO 2')
 1030 FORMAT (I8)
 1035 FORMAT ('ERROR',I6,' OPENING THE TERMINAL')
 1100 FORMAT ('Enter flag command number range to be undone: 1 to',I5)
 1210 FORMAT (A,I5,' OUT OF RANGE',I3,' to',I3)
 1211 FORMAT ('ANTENNA',I4,' NOT IN LIST')
 1220 FORMAT ('Enter ',I2,' antenna numbers to display')
 1221 FORMAT ('WARNING: CURRENT IF',I3,' OUTSIDE FLAG RANGE',2I3)
      END
      SUBROUTINE EDITTY (OPCODE, OBJECT, IERR)
C-----------------------------------------------------------------------
C   edit a TY table edit object   --- OR ---
C   edit a UV data object with a TY table object
C   Inputs:
C      OPCODE   C*4    Operation:
C                         'INIT' start a new edit session
C                         'ABOR' close session, delete flag info
C                         'KILL' close session, save FC table
C                         'APPL' close session, put flag info in master
C      OBJECT   C*(*)  Open Edit object
C   Output:
C      IERR     I      Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C                         < 0 => dies by the users' hands
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OBJECT*(*)
      INTEGER   IERR
C
      INTEGER   NCOL1, NCOL2, NCOLS, NXPRT, NCOLB, NXPRTB
      PARAMETER (NCOL1 = 22, NCOL2 = 18, NCOLS = NCOL1+NCOL2)
      PARAMETER (NCOLB = 20)
      PARAMETER (NXPRT = 12, NXPRTB = 14)
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'EDIFCPS.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER   TYPE, DIM(7), SCRTCH(256), I, J, K, TVCOL, TVROWS(2),
     *   TIMLIM, CHS, TVWND(4), MINWIN(2), VERS, IG(8), BUTTON, DUMMY,
     *   TTY(2), ITRIM, IVAL(NPLT), NOZOOM(3), SVZOOM(3), ILAST, LT1,
     *   LT2, TVSTAT(16), GRSTAT(8), JERR, IFLN(2), ITEMP(2), NTITLE,
     *   OPTOKX(NXPRT), OPTOKB(NXPRTB), MSGSAV, AVAL(50), SIDSEP
      REAL      T(8), RDUM
      DOUBLE PRECISION DVAL(2)
      LOGICAL   EDAFND, DOPLOT, LOADIT, LEAVE(NCOLS), WANT
      CHARACTER STATUS*4, PROBLM*32, CDUMMY*1, CHOICS(NCOLS)*16,
     *   ISHELP*6, LIST1(NCOL1)*16, LIST2(NCOL2)*16, MSGBUF*72,
     *   DDNAME*18, INKT(4)*8, INEXT*2, LISTB(NCOLB)*16, REAZON*24,
     *   CHOICE*16, CHOICX(NXPRT)*16, SHORTX(NXPRT)*4, ALPHAX(NXPRT)*1,
     *   CHOICB(NXPRTB)*16, SHORTB(NXPRTB)*4, ALPHAB(NXPRTB)*1,
     *   SHOW(2)*16, TITLE*8, STOKES*4
      DATA INKT /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
      DATA LIST1 /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'RESTORE TIME', 'RESTORE AREA', 'SMOOTH TIME RNG',
     *   'REREAD TIME RNG', 'ENTER VALUES', 'ENTER SMOOTH PS',
     *   'ENTER TSYS RNG', 'ENTER TDIF RNG', 'SET SCAN LENGTH',
     *   'USE EXPERT MODE', 'HOLD TV LOAD', 'REPLOT',
     *   ' ', 'EXIT', 'ABORT'/
      DATA LISTB /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'ENTER TSYS RNG', 'ENTER TDIF RNG', 'SET SCAN LENGTH',
     *   'LIST FLAGS', 'UNDO FLAGS', 'REDO FLAGS', 'SET REASON',
     *   'USE EXPERT MODE', 'HOLD TV LOAD', 'REPLOT',
     *   ' ', 'EXIT', 'ABORT'/
      DATA LIST2 /'NEXT POL/IF', 'SWITCH POLARIZ', 'SWITCH ALL POL',
     *   'ENTER IF', 'SWITCH ALL IF', 'SWITCH ALL TIME',
     *   'SWITCH ALL SOURC', 'SWITCH ALL ANT', 'ENTER ANTENNA',
     *   'ENTER OTHER ANT', 'NEXT ANTENNA', 'PLOT ALL TIMES',
     *   'SELECT FRAME', 'NEXT FRAME', 'PREVIOUS FRAME',
     *   'SHOW TSYS/<T>-1', 'TV ZOOM', 'OFF ZOOM'/
      DATA SHOW /'SHOW TSYS', 'SHOW TSYS/<T>-1'/
      DATA CHOICX /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'NEXT POL/IF', 'NEXT ANTENNA', 'SELECT FRAME', 'SHOW TSYS',
     *   'SHOW TSYS/<T>-1'/
      DATA SHORTX /'Time', 'Rang', 'Belo', 'Abov', 'arEa', 'pOin',
     *   'Quik', 'nCor', 'Nant', 'Fram', 'tSys', 'tDif'/
      DATA ALPHAX /'T', 'R', 'B', 'A', 'E', 'O', 'Q', 'C', 'N', 'F',
     *   'S', 'D'/
      DATA CHOICB /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'LIST FLAGS', 'UNDO FLAGS', 'NEXT POL/IF', 'NEXT ANTENNA',
     *   'SELECT FRAME', 'SHOW TSYS', 'SHOW TSYS/<T>-1'/
      DATA SHORTB /'Time', 'Rang', 'Belo', 'Abov', 'arEa', 'pOin',
     *   'Quik', 'List', 'Undo', 'nCor', 'Nant', 'Fram', 'tSys', 'tDif'/
      DATA ALPHAB /'T', 'R', 'B', 'A', 'E', 'O', 'Q', 'L', 'U', 'C',
     *   'N', 'F', 'S', 'D'/
      DATA LEAVE /NCOLS*.TRUE./
C-----------------------------------------------------------------------
      TTY(1) = 5
      TTY(2) = 0
      WASACR = .FALSE.
      WASXCR = .FALSE.
      ALLSOR = .FALSE.
C                                       plot scale ranges
      CALL RFILL (8, 1.0, RSCALE)
C                                       Init the functions
      IF (OPCODE.EQ.'INIT') THEN
         FLAGED = .TRUE.
         APOPEN = .FALSE.
         FCREAT = .FALSE.
         DO2ND = .FALSE.
         DOERRB = .FALSE.
         DO 5 I = 1,MAXIF
            IFWASN(I) = .FALSE.
 5          CONTINUE
         FLGMSG = .TRUE.
         SCMAXS = SCMAXX
         ILAST = 0
         NEWPLT = .TRUE.
         DTYPE(1) = 'T sys'
         DTYPE(2) = 'Tsys/<T>-1'
         DTYPE(3) = '????????'
         DTYPE(4) = '????????'
         DUNITS(1) = 'K'
         DUNITS(2) = 'Ratio'
         DUNITS(3) = '*******'
         DUNITS(4) = '*******'
         CALL RFILL (8, 1.0, DPLSCL)
         ALLPOL = .FALSE.
         ALLTIM = .FALSE.
         ALLIF = -1
         SMTYPE = '2PT'
         SMTIME(1) = 300.
         SMTIME(2) = 300.
         SMTIME(3) = 20.
         LOADIT = .TRUE.
         PNDING = .TRUE.
C                                       get attached class names
         PROBLM = OBJECT
         CALL EDIGET (OBJECT, 'UVMASTER', TYPE, DIM, DUMMY, UVMAST,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL SECSLT (UVMAST, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL EDIGET (OBJECT, 'TBEDIT', TYPE, DIM, DUMMY, TBEDIT, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDIGET (OBJECT, 'TVDEVICE', TYPE, DIM, DUMMY, TVNAME,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDIGET (OBJECT, 'DOUVFLAG', TYPE, DIM, UVFLAG, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         UVDAT2 = ' '
C                                       Make sure that the table is
C                                       sorted correctly
         CALL TBLSRT (TBEDIT, 'TIME', 'ANTENNA NO.', IERR)
         IF (IERR.NE.0) GO TO 999
         PROBLM = TBEDIT
C                                       Open table object: get adverbs
         STATUS = 'READ'
         CALL TABOPN (TBEDIT, STATUS, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Get a source list
         CALL EDSLST (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Create FCFILE table object
         IF (UVFLAG) THEN
            SMTYPE = ' '
            FCFILE = 'Flag Command table for TY edits'
            PROBLM = FCFILE
            CALL CREATE (FCFILE, 'TABLE', IERR)
            IF (IERR.NE.0) GO TO 980
            FCREAT = .TRUE.
C                                       copy adverbs to FCFILE
            CALL IN2OBJ (TBEDIT, 4, INKT, INKT, FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Extension type TBLTYPE
            INEXT = 'FC'
            DIM(1) = 2
            DIM(2) = 1
            CALL OPUT (FCFILE, 'TBLTYPE', OOACAR, DIM, DUMMY, INEXT,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       version always 1
            VERS = 1
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (FCFILE, 'VER', OOAINT, DIM, VERS, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       create the FC table file
            CALL OBFEXS (FCFILE, PREXIS, IERR)
            IERR = 0
            IF (PREXIS) THEN
               MSGTXT = '**** WARNING: USING A PRE-EXISTING FC TABLE' //
     *            ' ****'
               CALL MSGWRT (8)
               END IF
            CALL OFCINI (FCFILE, 'WRIT', FLGNUM, FCROW, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *         FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *         FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
            IF (IERR.NE.0) GO TO 980
            FLGNMX = FLGNUM
C                                       edit TY not UV
         ELSE
            FCFILE = ' '
            FLGNMX = 0
            END IF
C                                       get other adverbs
         PROBLM = TBEDIT
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'COMPARE', TYPE, DIM, DOCOMP, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            DOCOMP = .TRUE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'DO3COLOR', TYPE, DIM, RDUM, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            RDUM = -1.0
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'CROWDED', TYPE, DIM, CROWDP, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            CROWDP = .FALSE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         CROWDI = CROWDP
         IF (.NOT.CROWDP) RDUM = -1.0
         DO3COL = RDUM.GT.0.0
         AL3COL = RDUM.GT.1.5
         IS3COL = DO3COL
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'REASON', TYPE, DIM, DUMMY, REASON, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            REASON = ' '
            IERR = 0
            END IF
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'DOEXPERT', TYPE, DIM, EXPERT, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            EXPERT = .FALSE.
            IERR = 0
            END IF
         IF ((EXPERT) .AND. (.NOT.CROWDP)) THEN
            ALLIF = 1
            ALLPOL = .TRUE.
            END IF
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'FRQSEL', TYPE, DIM, FRQSEL, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (FRQSEL.EQ.0) FRQSEL = 1
         CALL TABGET (TBEDIT, 'VER', TYPE, DIM, VERS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'SUBARR', TYPE, DIM, SUBARR, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         SUBARR = MAX (1, SUBARR)
         CALL TABGET (TBEDIT, 'TIMERANG', TYPE, DIM, T, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         TSTART = T(1) + (T(2) + (T(3) + T(4)/60.) / 60.) / 24.
         TEND = T(5) + (T(6) + (T(7) + T(8)/60.) / 60.) / 24.
         IF (TEND.LE.TSTART) TEND = 999.
         CALL TABGET (TBEDIT, 'EQU_TIME', TYPE, DIM, DTIME, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (DTIME.LE.0.0) DTIME = 1 / 6000.0
         CALL TABGET (TBEDIT, 'GAP_TIME', TYPE, DIM, GTIME, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0 * DTIME)
         SMTIME(1) = MAX (GTIME * 60.0, 0.5)
         CALL TABGET (TBEDIT, 'ANTENNAS', TYPE, DIM, SCRTCH, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
C                                       parameters of TY
         CALL OGET (TBEDIT, 'NROW', TYPE, DIM, ROWMAX, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IFMAX = EIF - BIF + 1
         CALL OGET (TBEDIT, 'KEY.NO_POL', TYPE, DIM, POLMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (POLMAX.LE.0) POLMAX = 1
         IF (POLMAX.GT.2) THEN
             WRITE (MSGTXT,1000) POLMAX
             CALL MSGWRT (6)
             POLMAX = 2
             END IF
C                                       TY table does not say NO_ANT!
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OGET (TBEDIT, 'KEY.NO_ANT', TYPE, DIM, ANTMAX, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
C                                       resort to overkill
         IF ((IERR.NE.0) .OR. (ANTMAX.LE.0) .OR. (ANTMAX.GT.MAXANT))
     *      THEN
            CALL EDMAXA (TBEDIT, ANTMAX, IERR)
            IF (IERR.NE.0) ANTMAX = MAXANT
            END IF
         J = 0
         WANT = .TRUE.
         CALL FILL (MAXANT, 0, MSAMPS)
         CALL FILL (MAXANT, 0, ANTEN)
         DO 10 I = 1,ANTMAX
            ANTEN(I) = I
            IF (SCRTCH(I).NE.0) J = J + 1
            IF (SCRTCH(I).LT.0) WANT = .FALSE.
 10         CONTINUE
         IF (J.GT.0) THEN
            CALL FILL (MAXANT, 0, ANTEN)
            IF (WANT) THEN
               J = 0
               DO 20 I = 1,ANTMAX
                  IF (SCRTCH(I).GT.0) THEN
                     IF (J.GT.0) THEN
                        DO 15 K = 1,J
                           IF (SCRTCH(I).EQ.ANTEN(K)) GO TO 20
 15                        CONTINUE
                        END IF
                     J = J + 1
                     ANTEN(J) = SCRTCH(I)
                     END IF
 20               CONTINUE
            ELSE
               J = 0
               DO 30 I = 1,ANTMAX
                  DO 25 K = 1,ANTMAX
                     IF (I.EQ.ABS(SCRTCH(K))) GO TO 30
 25               CONTINUE
                  J = J + 1
                  ANTEN(J) = I
 30               CONTINUE
               END IF
            ANTMLX = J
         ELSE
            ANTMLX = ANTMAX
            END IF
C                                       build descriptor string
         CALL TABGET (TBEDIT, 'NAME', TYPE, DIM, DUMMY, DDNAME(:12),
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'CLASS', TYPE, DIM, DUMMY, DDNAME(13:),
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'IMSEQ', TYPE, DIM, J, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL NAMEST (DDNAME, J, DDSTR, DDSLEN)
         CALL TABGET (TBEDIT, 'TBLTYPE', TYPE, DIM, DUMMY, DDTYPE, IERR)
         IF (IERR.NE.0) GO TO 980
         DDSLEN = DDSLEN + 4
         DDSTR(DDSLEN:) = DDTYPE // ' VERS'
         DDSLEN = DDSLEN + 8
         WRITE (MSGBUF,1030) VERS
         CALL CHTRIM (MSGBUF, 8, MSGBUF, J)
         DDSTR(DDSLEN:) = MSGBUF(:J)
         DDSLEN = DDSLEN + 3 + J
         DDSTR(DDSLEN:) = 'FQID'
         DDSLEN = DDSLEN + 5
         WRITE (MSGBUF,1030) FRQSEL
         CALL CHTRIM (MSGBUF, 8, MSGBUF, J)
         DDSTR(DDSLEN:) = MSGBUF(:J)
         DDSLEN = DDSLEN + J - 1
C                                       close and reopen for update
         CALL TABCLO (TBEDIT, IERR)
         IF (IERR.NE.0) GO TO 980
         STATUS = 'WRIT'
         CALL TABOPN (TBEDIT, STATUS, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       basic parameters
         TIMEM = 0
         TIME1 = 1
         TIME2 = 0
         ALLIN = .FALSE.
         PIFNOW = -1
         MAXREC = 0
         POLNOW = 1
         IF (CROWDP) POLNOW = 0
         LTYPE = 1
         LTYPE2 = 0
         IF (DOCOMP) LTYPE2 = 2
         IFNOW = BIF
         IF (CROWDI) IFNOW = 0
C                                       no default range
         CALL RFILL (8, 0.0, DPIXR)
C                                       Open terminal for conversation
         CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *      IERR)
         IF (IERR.NE.0) THEN
            TTY(2) = 0
            WRITE (MSGTXT,1035) IERR
            CALL MSGWRT (8)
            PROBLM = 'The terminal'
            GO TO 980
            END IF
         TTY(2) = MAX (1, TTY(2))
C                                       Graphics: menu, menu back,
C                                       editing, editdata, flagged data,
C                                       extra data, full data
C                                       2 3 4 1 6 5 7
         GRSEL(1) = 6
         GRSEL(2) = 3
         GRSEL(3) = 4
         GRSEL(4) = 1
         GRSEL(5) = 5
         GRSEL(6) = 2
         GRSEL(7) = 0
C                                       learn about TV
         PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         MINWIN(1) = (36 + CEDG) * CSIZE(1) + 300
         MINWIN(2) = 37 * CSIZE(2) + 150
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
C                                       off all TV channels
         CALL TVDOPR (TVNAME, 'HOLD', I, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDOPR (TVNAME, 'INIT', I, IERR)
         IF (IERR.NE.0) GO TO 980
         I = 1
         CALL TVDOPR (TVNAME, 'TVOF', I, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       clear and off all graphics
         DO 35 I = 1,NGRPH-1
            CALL TVDOPR (TVNAME, 'GRON', I, IERR)
            IF (IERR.NE.0) GO TO 980
 35         CONTINUE
         CALL TVDFUN (TVNAME, 'OFFZ', 0, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDZOM (TVNAME, 'READ', NOZOOM, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (DO3COL) THEN
            CALL ED3CIN (IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Get some data
         WASFLG = -1
         PROBLM = TBEDIT
         CALL EDGTTY (IERR)
         IF (IERR.NE.0) GO TO 980
         APOPEN = .TRUE.
         IF (POLMAX.LE.1) THEN
            CROWDP = .FALSE.
            POLNOW = 1
            END IF
         IF (.NOT.ALLIN) THEN
            CROWDI = .FALSE.
            IFNOW = BIF
            END IF
C                                       get initial antennas to plot
         NUMPLT = 0
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'ANTS2USE', TYPE, DIM, AVAL, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            CALL FILL (50, 0, AVAL)
            IERR = 0
            END IF
         DO 40 I = 1,NPLT
            IF ((AVAL(I).GT.0) .AND. (AVAL(I).LE.ANTMAX) .AND.
     *         (EDAFND(AVAL(I), ANTMLX,ANTEN)) .AND.
     *         (MSAMPS(AVAL(I)).GT.0)) THEN
               NUMPLT = NUMPLT + 1
               PLTAN(NUMPLT) = AVAL(I)
               END IF
 40         CONTINUE
         IF (NUMPLT.LE.0) THEN
            DO 45 I = 1,ANTMLX
               IF (MSAMPS(ANTEN(I)).GT.0) THEN
                  NUMPLT = NUMPLT + 1
                  IF (NUMPLT.LE.NPLT) PLTAN(NUMPLT) = ANTEN(I)
                  END IF
 45            CONTINUE
            NUMPLT = MIN (NUMPLT, 3)
            END IF
         ALLANT = PLTAN(1)
C                                       plot the data
         PROBLM = TVNAME
         CALL EDPLOT (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       set parameters for menu
         TVCOL = 2
         IF (UVFLAG) THEN
            TVROWS(1) = NCOLB
            DO 50 I = 1,NCOLB
               CHOICS(I) = LISTB(I)
 50            CONTINUE
         ELSE
            TVROWS(1) = NCOL1
            DO 55 I = 1,NCOL1
               CHOICS(I) = LIST1(I)
 55            CONTINUE
            END IF
         J = TVROWS(1)
         IF (POLMAX*(EIF-BIF+1).GT.2) THEN
            J = J + 1
            CHOICS(J) = LIST2(1)
            END IF
         IF (POLMAX.GT.1) THEN
            J = J + 1
            CHOICS(J) = LIST2(2)
            J = J + 1
            CHOICS(J) = LIST2(3)
            END IF
         IF (EIF.GT.BIF) THEN
            J = J + 1
            CHOICS(J) = LIST2(4)
            IF (DIMIF.GT.1) THEN
               J = J + 1
               CHOICS(J) = LIST2(5)
               END IF
            END IF
         DO 60 I = 6,NCOL2
            J = J + 1
            CHOICS(J) = LIST2(I)
 60         CONTINUE
         TVROWS(2) = J - TVROWS(1)
         IF (UVFLAG) THEN
            ISHELP = 'EDITY'
         ELSE
            ISHELP = 'EDITS'
            END IF
         TIMLIM = 0
         IG(1) = GRSEL(1)
         IG(2) = GRSEL(2)
         CALL FILL (NXPRT, 1, OPTOKX)
         CALL FILL (NXPRTB, 1, OPTOKB)
         OPTOKX(NXPRT-1) = -1
         OPTOKB(NXPRTB-1) = -1
         IF ((POLMAX.LE.1) .AND. (EIF.EQ.BIF)) THEN
            OPTOKX(NXPRT-4) = -1
            OPTOKB(NXPRTB-4) = -1
            END IF
C                                       LOOP POINT
C                                       window still big enough?
 100     PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
         I = LTVWND(4) - TOPLOT + 5 * CSIZE(2) - 1
         IF (NEWPLT) THEN
            CALL TVDOKA (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            GRSTAT(IG(1)) = 1
            CALL TVDRST (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            ILAST = I
            END IF
         IF (PREXIS) THEN
            CHOICE = 'REDO FLAGS'
         ELSE IF ((EXPERT) .AND. (UVFLAG)) THEN
            CALL TVDXPR (TVNAME, -1, TVCOL, TVROWS, IG, I, CHOICS,
     *         LEAVE, TTY, NXPRTB, CHOICB, SHORTB, ALPHAB, OPTOKB,
     *         CHOICE, IERR)
            PROBLM = 'The terminal'
            IF (IERR.NE.0) GO TO 980
            J = TVROWS(1) + TVROWS(2)
            CALL EDCFND (CHOICE, J, CHOICS, CHS)
         ELSE IF (EXPERT) THEN
            CALL TVDXPR (TVNAME, -1, TVCOL, TVROWS, IG, I, CHOICS,
     *         LEAVE, TTY, NXPRT, CHOICX, SHORTX, ALPHAX, OPTOKX,
     *         CHOICE, IERR)
            PROBLM = 'The terminal'
            IF (IERR.NE.0) GO TO 980
            J = TVROWS(1) + TVROWS(2)
            CALL EDCFND (CHOICE, J, CHOICS, CHS)
         ELSE
            TITLE = ' '
            NTITLE = 0
            SIDSEP = 7
            CALL TVDMEN (TVNAME, -1, TVCOL, TVROWS, IG, I, SIDSEP,
     *         ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHS,
     *         BUTTON, IERR)
            IF (IERR.NE.0) GO TO 980
            CHOICE = CHOICS(CHS)
            END IF
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
         IF (NEWPLT) ILAST = 0
C                                       Do something:
         IF (LOADIT) DOPLOT = .FALSE.
C                                       load/hold
         IF ((CHOICE.EQ.'HOLD TV LOAD') .OR.
     *      (CHOICE.EQ.'DO TV LOAD')) THEN
            IF (LOADIT) THEN
               CHOICS(CHS) = 'DO TV LOAD'
            ELSE
               CHOICS(CHS) = 'HOLD TV LOAD'
               END IF
            ILAST = 0
            LOADIT = .NOT.LOADIT
C                                       flag interaction
         ELSE IF (CHOICE(:5).EQ.'FLAG ') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (CHOICE.EQ.'FLAG TIME') THEN
               CALL EDFLTI (IERR)
            ELSE IF (CHOICE.EQ.'FLAG TIME RANGE') THEN
               CALL EDFLTR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG BELOW') THEN
               CALL EDFLFB (IERR)
            ELSE IF (CHOICE.EQ.'FLAG ABOVE') THEN
               CALL EDFLFA (IERR)
            ELSE IF (CHOICE.EQ.'FLAG AREA') THEN
               CALL EDFLAR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG POINT') THEN
               CALL EDFLPT (IERR)
            ELSE IF (CHOICE.EQ.'FLAG QUICKLY') THEN
               CALL EDFLQU (IERR)
            ELSE
               CALL EDSTUB ('FLAG', CHOICE, OBJECT)
               END IF
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       list FC table
         ELSE IF (CHOICE.EQ.'LIST FLAGS') THEN
            CALL EDFCLI (IERR)
C                                       Undo flags in FC table
         ELSE IF (CHOICE.EQ.'UNDO FLAGS') THEN
            IF (FLGNMX.LE.1) THEN
               IFLN(1) = FLGNMX
               IFLN(2) = FLGNMX
               IERR = 0
            ELSE
               WRITE (MSGBUF,1100) FLGNMX
               CALL INQINT (TTY, MSGBUF, 2, IFLN, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
               IF (IFLN(2).LT.IFLN(1)) IFLN(2) = IFLN(1)
               END IF
            IF ((IERR.EQ.0) .AND. (IFLN(1).GT.0)) THEN
               PROBLM = TVNAME
               CALL EDFCUN (IFLN, IERR)
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Redo flags in FC table
         ELSE IF (CHOICE.EQ.'REDO FLAGS') THEN
            CALL EDFCDO (IERR)
            PREXIS = .FALSE.
C                                       interp at flag point
         ELSE IF (CHOICE.EQ.'SMOOTH TIME RNG') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL EDSMTR (IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Re-read at flag point
         ELSE IF (CHOICE.EQ.'REREAD TIME RNG') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL EDRRTR (IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Re-read at flag point
         ELSE IF (CHOICE.EQ.'ENTER VALUES') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL EDVENT (TTY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Unflag
         ELSE IF (CHOICE(:8).EQ.'RESTORE ') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (CHOICE.EQ.'RESTORE TIME') THEN
               CALL EDRSTI (IERR)
            ELSE IF (CHOICE.EQ.'RESTORE AREA') THEN
               CALL EDRSAR (IERR)
            ELSE
               CALL EDSTUB ('RESTORE', CHOICE, OBJECT)
               END IF
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'ENTER ANTENNA') THEN
            MSGBUF = 'Enter number of antenna to be edited'
            CALL INQINT (TTY, MSGBUF, 1, I, IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.LT.0) THEN
               I = PLTAN(1)
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
            IF ((I.LE.0) .OR. (I.GT.ANTMAX)) THEN
               WRITE (MSGTXT,1210) 'ANTENNA', I, 1, ANTMAX
               CALL MSGWRT (6)
            ELSE IF ((EDAFND (I, ANTMLX, ANTEN)) .AND. (MSAMPS(I).GT.0))
     *         THEN
               IF (I.NE.PLTAN(1)) THEN
                  LT1 = 0
                  LT2 = 0
                  DO 105 J = 2,NUMPLT
                     IF (PLTAN(J).EQ.I) LT1 = J
                     IF (PLTAN(J).EQ.PLTAN(1)) LT2 = J
 105                 CONTINUE
                  IF (LT1.GT.0) THEN
                     IF (LT2.EQ.0) THEN
                        PLTAN(LT1) = PLTAN(1)
                     ELSE
                        CALL COPY (NUMPLT-LT1, PLTAN(LT1+1), PLTAN(LT1))
                        NUMPLT = NUMPLT - 1
                        END IF
                     END IF
                  PLTAN(1) = I
                  IF (ALLANT.GT.0) ALLANT = PLTAN(1)
                  DOPLOT = .TRUE.
                  PNDING = .TRUE.
                  END IF
            ELSE
               WRITE (MSGTXT,1211) I
               CALL MSGWRT (6)
               END IF
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'NEXT ANTENNA') THEN
            I = PLTAN(1)
 110        I = I + 1
               IF (I.GT.ANTMAX) I = 1
               IF (MSAMPS(I).LE.0) GO TO 110
               IF (.NOT.(EDAFND (I, ANTMLX, ANTEN))) GO TO 110
            IF (I.NE.PLTAN(1)) THEN
               LT1 = 0
               LT2 = 1
               DO 115 J = 2,NUMPLT
 111              IF (PLTAN(J).EQ.I) THEN
                     CALL COPY (NUMPLT-J, PLTAN(J+1), PLTAN(J))
                     PLTAN(NUMPLT) = 0
                     GO TO 111
                     END IF
                  LT1 = MAX (LT1, PLTAN(J))
                  IF (PLTAN(J).GT.0) LT2 = J
 115              CONTINUE
               PLTAN(1) = I
               IF (ALLANT.GT.0) ALLANT = PLTAN(1)
C                                       add to other ants
 120           IF (LT2.LT.NUMPLT) THEN
                  I = LT1
                  IF (I.EQ.0) I = PLTAN(1)
                  DO 125 K = 1,ANTMAX
                     I = I + 1
                     IF (I.GT.ANTMAX) I = 1
C                                       good antenna - used already?
                     IF ((EDAFND (I, ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(I).GT.0)) THEN
                        DO 121 J = 1,LT2
                           IF (I.EQ.PLTAN(J)) GO TO 125
 121                       CONTINUE
C                                       found a new one
                        LT2 = LT2 + 1
                        PLTAN(LT2) = I
                        LT1 = MAX (LT1, I)
                        GO TO 120
                        END IF
 125                 CONTINUE
                  NUMPLT = LT2
                  END IF
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Other ants
         ELSE IF (CHOICE.EQ.'ENTER OTHER ANT') THEN
            I = NPLT - 1
            WRITE (MSGBUF,1220) I
            CALL INQINT (TTY, MSGBUF, I, IVAL(2), IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.EQ.0) THEN
               K = NUMPLT
               NUMPLT = 1
               DO 135 I = 2,NPLT
                  IF ((IVAL(I).GT.0) .AND. (IVAL(I).LE.ANTMAX)) THEN
                     IF ((EDAFND (IVAL(I), ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(IVAL(I)).GT.0)) THEN
                        NUMPLT = NUMPLT + 1
                        PLTAN(NUMPLT) = IVAL(I)
                        END IF
                     END IF
 135              CONTINUE
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               IF (K.NE.NUMPLT) THEN
                  CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
                  IF (IERR.NE.0) GO TO 980
                  NEWPLT = .TRUE.
                  ILAST = 0
                  END IF
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Select IF
         ELSE IF (CHOICE.EQ.'ENTER IF') THEN
            IF ((EIF.EQ.BIF+1) .AND. (.NOT.CROWDI)) THEN
               I = 2 * BIF + 1 - IFNOW
               IERR = 0
            ELSE IF ((EIF.EQ.BIF+1) .AND. (CROWDI)) THEN
               IF (IFNOW.EQ.0) THEN
                  I = BIF
               ELSE IF (IFNOW.EQ.EIF) THEN
                  I = 0
               ELSE
                  I = EIF
                  END IF
               IERR = 0
            ELSE
               MSGBUF = 'Enter number of IF to be plotted and edited'
               CALL INQINT (TTY, MSGBUF, 1, I, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
               IF (IERR.LT.0) THEN
                  I = IFNOW
                  MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                  CALL MSGWRT (6)
                  IERR = 0
                  END IF
               END IF
            IF (((I.LT.BIF) .OR. (I.GT.EIF)) .AND. ((I.NE.0) .OR.
     *         (.NOT.CROWDI))) THEN
               WRITE (MSGTXT,1210) 'IF', I, BIF, EIF
               CALL MSGWRT (6)
            ELSE IF (I.NE.IFNOW) THEN
               PNDING = .TRUE.
               PIFNOW = IFNOW
               IFNOW = I
               IF ((ALLIF.EQ.0) .AND. ((IFNOW.LT.FGIFRS(1)) .OR.
     *            (IFNOW.GT.FGIFRS(2)))) THEN
                  WRITE (MSGTXT,1221) IFNOW, FGIFRS
                  CALL MSGWRT (7)
                  END IF
               IF (.NOT.ALLIN) THEN
                  PROBLM = TBEDIT
                  CALL EDGTTY (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               DOPLOT = .TRUE.
               END IF
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Smooth parameters
         ELSE IF (CHOICE.EQ.'ENTER SMOOTH PS') THEN
            MSGBUF = 'Enter smooth type (2PT BOX MWF 2PTH GAUS LINE '
     *         // 'EXP) left justified'
 136        CALL INQSTR (TTY, MSGBUF, 4, STATUS, IERR)
            IF (IERR.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 136
               END IF
            CALL CHLTOU (4, STATUS)
            IF ((IERR.EQ.0) .AND. ((STATUS.EQ.'2PT') .OR.
     *         (STATUS.EQ.'BOX') .OR. (STATUS.EQ.'MWF') .OR.
     *         (STATUS.EQ.'2PTH') .OR. (STATUS.EQ.'GAUS') .OR.
     *         (STATUS.EQ.'LINE') .OR. (STATUS.EQ.'EXP '))) THEN
               SMTYPE = STATUS
               MSGBUF = 'Enter smooth box full width in seconds'
               CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
               IF ((IERR.EQ.0) .AND. (I.GT.0) .AND. (I.LT.86400)) THEN
                  SMTIME(2) = DVAL(1)
                  IF ((SMTYPE.EQ.'GAUS') .OR. (SMTYPE.EQ.'LINE') .OR.
     *               (SMTYPE.EQ.'EXP ')) THEN
                     MSGBUF = 'Enter full width half max in seconds'
                     CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
                     IF ((IERR.EQ.0) .AND. (I.GT.0) .AND. (I.LT.86400))
     *                  THEN
                        SMTIME(3) = DVAL(1)
                        CALL EDPLST (GRSEL(6), IERR)
                     ELSE
                        MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                        CALL MSGWRT (6)
                        END IF
                     END IF
               ELSE
                  MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                  CALL MSGWRT (6)
                  END IF
            ELSE IF ((IERR.NE.0) .OR. (STATUS.NE.' ')) THEN
               MSGTXT = 'THERE WAS A PROBLEM: SMTYPE KEPT AS ' // SMTYPE
               CALL MSGWRT (6)
               END IF
            IERR = MAX (IERR,0)
C                                       scan length for diff
         ELSE IF (CHOICE.EQ.'SET SCAN LENGTH') THEN
            MSGBUF = 'Enter ''scan'' length in seconds'
            CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
            IF (IERR.GT.0) GO TO 980
C                                       recompute the differences
            IF ((IERR.EQ.0) .AND. (DVAL(1).GT.0) .AND.
     *         (DVAL(1).LT.86400) .AND. (DVAL(1).NE.SMTIME(1))) THEN
               SMTIME(1) = DVAL(1)
               FLAGED = .TRUE.
               CALL EDPLST (GRSEL(6), IERR)
               DOPLOT = (LTYPE.EQ.3) .OR. (DOCOMP .AND. (LTYPE2.EQ.3))
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Set reason
         ELSE IF (CHOICE.EQ.'SET REASON') THEN
            MSGBUF = 'Enter new reason, left justified'
 137        CALL INQSTR (TTY, MSGBUF, 24, REAZON, IERR)
            IF (IERR.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 137
               END IF
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF ((IERR.EQ.0) .AND. (REAZON.NE.'-')) THEN
               REASON = REAZON
               I = ITRIM (REASON)
               I = MAX (1, I)
               MSGTXT = 'Using reason = ''' // REASON(:I) // ''''
               CALL MSGWRT (2)
               END IF
            IERR = MAX (0, IERR)
C                                       Set ranges
         ELSE IF (CHOICE(:7).EQ.'ENTER T') THEN
            I = 2
            IF (CHOICE.EQ.'ENTER TSYS RNG') I = 1
            J = ITRIM (DTYPE(I))
            MSGBUF = 'Enter ' // DTYPE(I)(:J) // ' display range in ' //
     *         DUNITS(I)
            CALL INQFLT (TTY, MSGBUF, 2, DVAL, IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.EQ.0) THEN
               DPIXR(1,I) = DVAL(1) * DPLSCL(I)
               DPIXR(2,I) = DVAL(2) * DPLSCL(I)
               IF ((I.EQ.LTYPE) .OR. (DOCOMP .AND. (I.EQ.LTYPE2))) THEN
                  DOPLOT = .TRUE.
                  PNDING = .TRUE.
                  END IF
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Switch polarization
         ELSE IF (CHOICE.EQ.'SWITCH POLARIZ') THEN
            IF (CROWDP) THEN
               POLNOW = MOD (POLNOW+1, 3)
            ELSE
               POLNOW = 3 - POLNOW
               END IF
            DOPLOT = .TRUE.
            PNDING = .TRUE.
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Next Correlator
         ELSE IF (CHOICE.EQ.'NEXT POL/IF') THEN
            PNDING = .TRUE.
            IF (POLNOW.LT.POLMAX) THEN
               POLNOW = POLNOW + 1
               IFNOW = MAX (BIF, IFNOW)
            ELSE
               POLNOW = 1
               IF (CROWDP) POLNOW = 0
               PIFNOW = IFNOW
               IFNOW = MAX (BIF, IFNOW+1)
               IF (IFNOW.GT.EIF) THEN
                  IFNOW = BIF
                  IF (CROWDI) IFNOW = 0
                  END IF
               PROBLM = TBEDIT
               IF (.NOT.ALLIN) THEN
                  CALL EDGTTY (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               END IF
            DOPLOT = .TRUE.
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Switch all ant
         ELSE IF (CHOICE.EQ.'SWITCH ALL ANT') THEN
            IF (ALLANT.GT.0) THEN
               ALLANT = 0
            ELSE
               ALLANT = PLTAN(1)
               END IF
            CALL EDPLST (GRSEL(6), IERR)
C                                       Switch all pol
         ELSE IF (CHOICE.EQ.'SWITCH ALL POL') THEN
            IF (POLMAX.GT.1) THEN
               ALLPOL = .NOT.ALLPOL
               CALL EDPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all IF
         ELSE IF (CHOICE.EQ.'SWITCH ALL IF') THEN
            IF (DIMIF.GT.1) THEN
               IF ((DIMIF.GT.2) .AND. (ALLIN)) THEN
                  ALLIF = ALLIF + 1
                  IF (ALLIF.EQ.2) THEN
                     ALLIF = -1
                  ELSE IF (ALLIF.EQ.0) THEN
                     MSGBUF = 'Enter IF number range to flag'
                     CALL INQINT (TTY, MSGBUF, 2, ITEMP, IERR)
                     PROBLM = 'The terminal'
                     IF (IERR.GT.0) GO TO 980
                     IF (IERR.EQ.0) THEN
                        IF ((ITEMP(1).EQ.0) .AND. (ITEMP(2).EQ.0)) THEN
                           ALLIF = -1
                        ELSE IF ((ITEMP(2).LT.ITEMP(1)) .OR.
     *                     (ITEMP(1).LT.BIF) .OR. (ITEMP(1).GT.EIF) .OR.
     *                     (ITEMP(2).LT.BIF) .OR. (ITEMP(2).GT.EIF))
     *                     THEN
                           IERR = -1
                        ELSE
                           FGIFRS(1) = ITEMP(1)
                           FGIFRS(2) = ITEMP(2)
                           IF ((IFNOW.LT.FGIFRS(1)) .OR.
     *                        (IFNOW.GT.FGIFRS(2))) THEN
                              WRITE (MSGTXT,1221) IFNOW, FGIFRS
                              CALL MSGWRT (7)
                              END IF
                           END IF
                        END IF
                     IF (IERR.LT.0) THEN
                        I = IFNOW
                        MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                        CALL MSGWRT (6)
                        IERR = 0
                        END IF
                     END IF
               ELSE
                  ALLIF = -ALLIF
                  END IF
               CALL EDPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all times
         ELSE IF (CHOICE.EQ.'SWITCH ALL TIME') THEN
            ALLTIM = .NOT.ALLTIM
            CALL EDPLST (GRSEL(6), IERR)
C                                       Switch all source
         ELSE IF (CHOICE.EQ.'SWITCH ALL SOURC') THEN
            ALLSOR = .NOT.ALLSOR
            CALL EDPLST (GRSEL(6), IERR)
C                                       TV zoom
         ELSE IF (CHOICE.EQ.'TV ZOOM') THEN
            CALL OTVZOM (TVNAME, IERR)
            CALL TVDZOM (TVNAME, 'READ', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Off zoom
         ELSE IF (CHOICE.EQ.'OFF ZOOM') THEN
            CALL OTVOFZ (TVNAME, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'READ', SVZOOM, IERR)
C                                       expert mode switch
         ELSE IF (CHOICE.EQ.'USE EXPERT MODE') THEN
            EXPERT = .NOT.EXPERT
C                                       full plot
         ELSE IF (CHOICE.EQ.'PLOT ALL TIMES') THEN
            IF ((TIME1.GT.1) .OR. (TIME2.LT.TIMEM)) THEN
               TIME1 = 1
               TIME2 = TIMEM-1
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Select frame
         ELSE IF (CHOICE.EQ.'SELECT FRAME') THEN
            IF ((TIME1.GT.1) .OR. (TIME2.LT.TIMEM-1) .OR. PNDING) THEN
               TIME1 = 1
               TIME2 = TIMEM-1
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL EDFRAM (IERR)
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Next frame
         ELSE IF (CHOICE.EQ.'NEXT FRAME') THEN
            LT2 = MIN (2*TIME2-TIME1, TIMEM-1)
            LT1 = LT2 - TIME2 + TIME1
            IF ((TIME1.NE.LT1) .OR. (TIME2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               TIME1 = LT1
               TIME2 = LT2
               END IF
C                                       Previous frame
         ELSE IF (CHOICE.EQ.'PREVIOUS FRAME') THEN
            LT1 = MAX (2*TIME1-TIME2, 1)
            LT2 = LT1 + TIME2 - TIME1
            IF ((TIME1.NE.LT1) .OR. (TIME2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               TIME1 = LT1
               TIME2 = LT2
               END IF
C                                       Show a type
         ELSE IF (CHOICE(:9).EQ.'SHOW TSYS') THEN
            I = LTYPE
            IF (CHOICE(10:).EQ.' ') LTYPE = 1
            IF (CHOICE(10:).EQ.'/<T>-1') LTYPE = 2
            CHOICS(CHS) = SHOW(3-LTYPE)
            IF (DOCOMP) LTYPE2 = 3 - LTYPE
            IF (UVFLAG) THEN
               OPTOKB(NXPRTB-2+I) = 1
               OPTOKB(NXPRTB-2+LTYPE) = -1
            ELSE
                OPTOKX(NXPRT-2+I) = 1
                OPTOKX(NXPRT-2+LTYPE) = -1
                END IF
            IF ((I.NE.LTYPE) .OR.(((LTYPE.EQ.2) .OR. (DOCOMP .AND.
     *         (LTYPE2.EQ.2))) .AND. (FLAGED))) THEN
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       REPLOT
         ELSE IF (CHOICE.EQ.'REPLOT') THEN
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       EXIT
         ELSE IF (CHOICE.EQ.'EXIT') THEN
            IERR = 0
            GO TO 995
C                                       ABORT
         ELSE IF (CHOICE.EQ.'ABORT') THEN
            IERR = -1
            GO TO 995
            END IF
         IF ((IERR.LE.0) .AND. (DOPLOT)) THEN
            IF (LOADIT) THEN
               CALL EDPLOT (IERR)
            ELSE IF (PNDING) THEN
               CALL EDPLST (GRSEL(6), IERR)
               END IF
            PROBLM = TVNAME
            END IF
         IF (IERR.GT.0) GO TO 980
         GO TO 100
C                                       apply result
      ELSE IF (OPCODE.EQ.'APPL') THEN
         PROBLM = TBEDIT
         PIFNOW = IFNOW
         IFNOW = -1
         CALL EDGTTY (IERR)
         IF (IERR.NE.0) GO TO 980
         IF (UVFLAG) THEN
            CALL EDFCAP (UVMAST, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TABZAP (FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'EDGTTY', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 800 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 800        CONTINUE
         IF (DO3COL) THEN
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+1, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+2, IERR)
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       do not apply, don't destroy
      ELSE IF ((OPCODE.EQ.'ABOR') .OR. (OPCODE.EQ.'KILL')) THEN
         PROBLM = TBEDIT
         IF ((UVFLAG) .AND. (FCREAT) .AND. (OPCODE.NE.'KILL')) THEN
            CALL TABZAP (FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'EDGTTY', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 810 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 810        CONTINUE
         IF (DO3COL) THEN
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+1, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+2, IERR)
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       ??
      ELSE
         IERR = 2
         CALL EDSTUB ('EDITTY', OPCODE, OBJECT)
         END IF
      GO TO 995
C                                       error
 980  MSGTXT = 'EDITTY: ERROR WITH OBJECT ' // PROBLM
      CALL MSGWRT (7)
C
 995  IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NO_POL = ',I3,' LIMITED HERE TO 2')
 1030 FORMAT (I8)
 1035 FORMAT ('ERROR',I6,' OPENING THE TERMINAL')
 1100 FORMAT ('Enter flag command number range to be undone: 1 to',I5)
 1210 FORMAT (A,I5,' OUT OF RANGE',I4,' to',I4)
 1211 FORMAT ('ANTENNA',I4,' NOT IN LIST')
 1220 FORMAT ('Enter ',I2,' antenna numbers to display')
 1221 FORMAT ('WARNING: CURRENT IF',I3,' OUTSIDE FLAG RANGE',2I3)
      END
      SUBROUTINE EDITSY (OPCODE, OBJECT, IERR)
C-----------------------------------------------------------------------
C   edit a SY table edit object   --- OR ---
C   edit a UV data object with a SY table object
C   Inputs:
C      OPCODE   C*4    Operation:
C                         'INIT' start a new edit session
C                         'ABOR' close session, delete flag info
C                         'KILL' close session, save FC table
C                         'APPL' close session, put flag info in master
C      OBJECT   C*(*)  Open Edit object
C   Output:
C      IERR     I      Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C                         < 0 => dies by the users' hands
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OBJECT*(*)
      INTEGER   IERR
C
      INTEGER   NCOL1, NCOL2, NCOLS, NXPRT, NCOLB, NXPRTB
      PARAMETER (NCOL1 = 27, NCOL2 = 30, NCOLS = NCOL1+NCOL2)
      PARAMETER (NCOLB = 26)
      PARAMETER (NXPRT = 14, NXPRTB = 16)
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'EDIFCPS.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER   TYPE, DIM(7), SCRTCH(256), I, J, K, TVCOL, TVROWS(2),
     *   TIMLIM, CHS, TVWND(4), MINWIN(2), VERS, IG(8), BUTTON, DUMMY,
     *   TTY(2), ITRIM, IVAL(NPLT), NOZOOM(3), SVZOOM(3), ILAST, LT1,
     *   LT2, TVSTAT(16), GRSTAT(8), JERR, IFLN(2), CHSHOW, CHSHO2,
     *   OPTOKX(NXPRT), OPTOKB(NXPRTB), MSGSAV, AVAL(50), DSK, CNO,
     *   CATB(256), TVER, ITEMP(2), NTITLE, SIDSEP
      REAL      T(8), RDUM
      DOUBLE PRECISION DVAL(2)
      LOGICAL   EDAFND, DOPLOT, LOADIT, LEAVE(NCOLS), WANT
      CHARACTER STATUS*4, PROBLM*32, CDUMMY*1, CHOICS(NCOLS)*16,
     *   ISHELP*6, LIST1(NCOL1)*16, LIST2(NCOL2)*16, MSGBUF*72,
     *   DDNAME*18, INKT(4)*8, INEXT*2, LISTB(NCOLB)*16, REAZON*24,
     *   CHOICE*16, CHOICX(NXPRT)*16, SHORTX(NXPRT)*4, ALPHAX(NXPRT)*1,
     *   CHOICB(NXPRTB)*16, SHORTB(NXPRTB)*4, ALPHAB(NXPRTB)*1,
     *   SHOW(8)*16, SHOW2(8)*16, TITLE*8, STOKES*4, TTYPE*2
      DATA INKT /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
      DATA LIST1 /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'RESTORE TIME', 'RESTORE AREA', 'SMOOTH TIME RNG',
     *   'REREAD TIME RNG', 'ENTER SMOOTH PS', 'ENTER PDIF RNG',
     *   'ENTER PSUM RNG', 'ENTER PGAIN RNG', 'ENTER TSYS RNG',
     *   'ENTER PD/P RNG','ENTER PS/P RNG', 'ENTER PG/P RNG',
     *   'ENTER TS/T RNG', 'SET SCAN LENGTH', 'USE EXPERT MODE',
     *   'HOLD TV LOAD', 'REPLOT', ' ', 'EXIT', 'ABORT'/
      DATA LISTB /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'ENTER PDIF RNG', 'ENTER PSUM RNG', 'ENTER PGAIN RNG',
     *   'ENTER TSYS RNG', 'ENTER PD/P RNG', 'ENTER PS/P RNG',
     *   'ENTER PG/P RNG', 'ENTER TS/T RNG', 'SET SCAN LENGTH',
     *   'LIST FLAGS', 'UNDO FLAGS', 'REDO FLAGS', 'SET REASON',
     *   'USE EXPERT MODE', 'HOLD TV LOAD', 'REPLOT', ' ', 'EXIT',
     *   'ABORT'/
      DATA LIST2 /'NEXT POL/IF', 'SWITCH POLARIZ', 'SWITCH ALL POL',
     *   'ENTER IF', 'SWITCH ALL IF', 'SWITCH ALL TIME',
     *   'SWITCH ALL SOURC', 'SWITCH ALL ANT', 'ENTER ANTENNA',
     *   'ENTER OTHER ANT', 'NEXT ANTENNA', 'PLOT ALL TIMES',
     *   'SELECT FRAME', 'NEXT FRAME', 'PREVIOUS FRAME', 'SHOW PSUM',
     *   'SHOW PGAIN', 'SHOW TSYS', 'SHOW PDIF/<P>-1',
     *   'SHOW PSUM/<P>-1', 'SHOW PGAI/<P>-1', 'SHOW TSYS/<T>-1',
     *   'SHOW ALSO PGAIN', 'SHOW ALSO TSYS', 'SHOW ALSO DPDIF',
     *   'SHOW ALSO DPSUM', 'SHOW ALSO DPGAIN', 'SHOW ALSO DTSYS',
     *   'TV ZOOM', 'OFF ZOOM'/
      DATA SHOW /'SHOW PDIF', 'SHOW PSUM', 'SHOW PGAIN', 'SHOW TSYS',
     *   'SHOW PDIF/<P>-1', 'SHOW PSUM/<P>-1', 'SHOW PGAIN/<P>-1',
     *   'SHOW TSYS/<T>-1'/
      DATA SHOW2 /'SHOW ALSO PDIF', 'SHOW ALSO PSUM', 'SHOW ALSO PGAIN',
     *   'SHOW ALSO TSYS', 'SHOW ALSO DPDIF', 'SHOW ALSO DPSUM',
     *   'SHOW ALSO DPGAIN', 'SHOW ALSO DTSYS'/
      DATA CHOICX /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'NEXT POL/IF', 'NEXT ANTENNA', 'SELECT FRAME', 'SHOW PDIF',
     *   'SHOW PSUM', 'SHOW TSYS', 'SHOW PGAIN'/
      DATA SHORTX /'Time', 'Rang', 'Belo', 'Abov', 'arEa', 'pOin',
     *   'Quik', 'nCor', 'Nant', 'Fram', 'pDif', 'pSum', 'pGai', 'tsYs'/
      DATA ALPHAX /'T', 'R', 'B', 'A', 'E', 'O', 'Q', 'C', 'N', 'F',
     *   'D', 'S', 'G', 'Y'/
      DATA CHOICB /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'LIST FLAGS', 'UNDO FLAGS', 'NEXT POL/IF', 'NEXT ANTENNA',
     *   'SELECT FRAME', 'SHOW PDIF', 'SHOW PSUM', 'SHOW PGAIN',
     *   'SHOW TSYS'/
      DATA SHORTB /'Time', 'Rang', 'Belo', 'Abov', 'arEa', 'pOin',
     *   'Quik', 'List', 'Undo', 'nCor', 'Nant', 'Fram', 'pDif', 'pSum',
     *   'pGai', 'tsYs'/
      DATA ALPHAB /'T', 'R', 'B', 'A', 'E', 'O', 'Q', 'L', 'U', 'C',
     *   'N', 'F', 'D', 'S', 'G', 'Y'/
      DATA LEAVE /NCOLS*.TRUE./
C-----------------------------------------------------------------------
      TTY(1) = 5
      TTY(2) = 0
      WASACR = .FALSE.
      WASXCR = .FALSE.
      ALLSOR = .FALSE.
C                                       plot scale ranges
      CALL RFILL (8, 1.0, RSCALE)
C                                       Init the functions
      IF (OPCODE.EQ.'INIT') THEN
         FLAGED = .TRUE.
         APOPEN = .FALSE.
         FCREAT = .FALSE.
         DO2ND = .FALSE.
         DOERRB = .FALSE.
         DO 5 I = 1,MAXIF
            IFWASN(I) = .FALSE.
 5          CONTINUE
         FLGMSG = .TRUE.
         SCMAXS = SCMAXX
         ILAST = 0
         NEWPLT = .TRUE.
         DTYPE(1) = 'Pdif'
         DTYPE(2) = 'Psum'
         DTYPE(3) = 'Pgain'
         DTYPE(4) = 'Tsys'
         DTYPE(5) = 'Pd/<Pd>'
         DTYPE(6) = 'Ps/<Ps>'
         DTYPE(7) = 'Pg/<Pg>'
         DTYPE(8) = 'Ts/<Ts>'
         DUNITS(1) = 'counts'
         DUNITS(2) = 'counts'
         DUNITS(3) = ' '
         DUNITS(4) = 'Kelvin'
         DUNITS(5) = 'Ratio'
         DUNITS(6) = 'Ratio'
         DUNITS(7) = 'Ratio'
         DUNITS(8) = 'Ratio'
         CALL RFILL (8, 1.0, DPLSCL)
         ALLPOL = .FALSE.
         ALLTIM = .FALSE.
         ALLIF = -1
         SMTYPE = '2PT'
         SMTIME(1) = 300.
         SMTIME(2) = 300.
         SMTIME(3) = 20.
         LOADIT = .TRUE.
         PNDING = .TRUE.
C                                       get attached class names
         PROBLM = OBJECT
         CALL EDIGET (OBJECT, 'UVMASTER', TYPE, DIM, DUMMY, UVMAST,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL SECSLT (UVMAST, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL EDIGET (OBJECT, 'TBEDIT', TYPE, DIM, DUMMY, TBEDIT, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDIGET (OBJECT, 'TVDEVICE', TYPE, DIM, DUMMY, TVNAME,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDIGET (OBJECT, 'DOUVFLAG', TYPE, DIM, UVFLAG, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         UVDAT2 = ' '
C                                       Make sure that the table is
C                                       sorted correctly
         CALL TBLSRT (TBEDIT, 'TIME', 'ANTENNA NO.', IERR)
         IF (IERR.NE.0) GO TO 999
         PROBLM = TBEDIT
C                                       Open table object: get adverbs
         STATUS = 'READ'
         CALL TABOPN (TBEDIT, STATUS, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Get a source list
         CALL EDSLST (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Create FCFILE table object
         IF (UVFLAG) THEN
            SMTYPE = ' '
            FCFILE = 'Flag Command table for SY edits'
            PROBLM = FCFILE
            CALL CREATE (FCFILE, 'TABLE', IERR)
            IF (IERR.NE.0) GO TO 980
            FCREAT = .TRUE.
C                                       copy adverbs to FCFILE
            CALL IN2OBJ (TBEDIT, 4, INKT, INKT, FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Extension type TBLTYPE
            INEXT = 'FC'
            DIM(1) = 2
            DIM(2) = 1
            CALL OPUT (FCFILE, 'TBLTYPE', OOACAR, DIM, DUMMY, INEXT,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       version always 1
            VERS = 1
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (FCFILE, 'VER', OOAINT, DIM, VERS, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       create the FC table file
            CALL OBFEXS (FCFILE, PREXIS, IERR)
            IERR = 0
            IF (PREXIS) THEN
               MSGTXT = '**** WARNING: USING A PRE-EXISTING FC TABLE' //
     *            ' ****'
               CALL MSGWRT (8)
               END IF
            CALL OFCINI (FCFILE, 'WRIT', FLGNUM, FCROW, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *         FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *         FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
            IF (IERR.NE.0) GO TO 980
            FLGNMX = FLGNUM
C                                       edit SY not UV
         ELSE
            FCFILE = ' '
            FLGNMX = 0
            END IF
C                                       get other adverbs
         PROBLM = TBEDIT
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'COMPARE', TYPE, DIM, DOCOMP, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            DOCOMP = .TRUE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'DO3COLOR', TYPE, DIM, RDUM, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            RDUM = -1.0
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'CROWDED', TYPE, DIM, CROWDP, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            CROWDP = .FALSE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         CROWDI = CROWDP
         IF (.NOT.CROWDP) RDUM = -1.0
         DO3COL = RDUM.GT.0.0
         AL3COL = RDUM.GT.1.5
         IS3COL = DO3COL
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'REASON', TYPE, DIM, DUMMY, REASON, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            REASON = ' '
            IERR = 0
            END IF
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'DOEXPERT', TYPE, DIM, EXPERT, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            EXPERT = .FALSE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         IF ((EXPERT) .AND. (.NOT.CROWDP)) THEN
            ALLIF = 1
            ALLPOL = .TRUE.
            END IF
         CALL TABGET (TBEDIT, 'FRQSEL', TYPE, DIM, FRQSEL, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (FRQSEL.EQ.0) FRQSEL = 1
         CALL TABGET (TBEDIT, 'VER', TYPE, DIM, VERS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'SUBARR', TYPE, DIM, SUBARR, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         SUBARR = MAX (1, SUBARR)
         CALL TABGET (TBEDIT, 'TIMERANG', TYPE, DIM, T, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         TSTART = T(1) + (T(2) + (T(3) + T(4)/60.) / 60.) / 24.
         TEND = T(5) + (T(6) + (T(7) + T(8)/60.) / 60.) / 24.
         IF (TEND.LE.TSTART) TEND = 999.
         CALL TABGET (TBEDIT, 'EQU_TIME', TYPE, DIM, DTIME, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (DTIME.LE.0.0) DTIME = 1 / 6000.0
         CALL TABGET (TBEDIT, 'GAP_TIME', TYPE, DIM, GTIME, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0 * DTIME)
         SMTIME(1) = MAX (GTIME * 60.0, 0.5)
         CALL TABGET (TBEDIT, 'ANTENNAS', TYPE, DIM, SCRTCH, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
C                                       parameters of SY
         CALL OGET (TBEDIT, 'NROW', TYPE, DIM, ROWMAX, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IFMAX = EIF - BIF + 1
         CALL OGET (TBEDIT, 'KEY.NO_POL', TYPE, DIM, POLMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (POLMAX.LE.0) POLMAX = 1
         IF (POLMAX.GT.2) THEN
             WRITE (MSGTXT,1000) POLMAX
             CALL MSGWRT (6)
             POLMAX = 2
             END IF
C                                       SY table does not say NO_ANT!
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OGET (TBEDIT, 'KEY.NO_ANT', TYPE, DIM, ANTMAX, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
C                                       resort to overkill
         IF ((IERR.NE.0) .OR. (ANTMAX.LE.0) .OR. (ANTMAX.GT.MAXANT))
     *      THEN
            CALL EDMAXA (TBEDIT, ANTMAX, IERR)
            IF (IERR.NE.0) ANTMAX = MAXANT
            END IF
C                                       get TCAL values
         J = 0
C                                       Look up table info
         CALL TBLKUP (TBEDIT, DSK, CNO, TTYPE, TVER, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Get CATBLK
         CALL OBHGET (TBEDIT, CATB, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL GETCDS (DSK, CNO, J, SUBARR, FRQSEL, CATB, TCAL, IERR)
         IF (IERR.NE.0) GO TO 980
         J = 0
         WANT = .TRUE.
         CALL FILL (MAXANT, 0, MSAMPS)
         CALL FILL (MAXANT, 0, ANTEN)
         DO 10 I = 1,ANTMAX
            ANTEN(I) = I
            IF (SCRTCH(I).NE.0) J = J + 1
            IF (SCRTCH(I).LT.0) WANT = .FALSE.
 10         CONTINUE
         IF (J.GT.0) THEN
            CALL FILL (MAXANT, 0, ANTEN)
            IF (WANT) THEN
               J = 0
               DO 20 I = 1,ANTMAX
                  IF (SCRTCH(I).GT.0) THEN
                     IF (J.GT.0) THEN
                        DO 15 K = 1,J
                           IF (SCRTCH(I).EQ.ANTEN(K)) GO TO 20
 15                        CONTINUE
                        END IF
                     J = J + 1
                     ANTEN(J) = SCRTCH(I)
                     END IF
 20               CONTINUE
            ELSE
               J = 0
               DO 30 I = 1,ANTMAX
                  DO 25 K = 1,ANTMAX
                     IF (I.EQ.ABS(SCRTCH(K))) GO TO 30
 25               CONTINUE
                  J = J + 1
                  ANTEN(J) = I
 30               CONTINUE
               END IF
            ANTMLX = J
         ELSE
            ANTMLX = ANTMAX
            END IF
C                                       build descriptor string
         CALL TABGET (TBEDIT, 'NAME', TYPE, DIM, DUMMY, DDNAME(:12),
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'CLASS', TYPE, DIM, DUMMY, DDNAME(13:),
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABGET (TBEDIT, 'IMSEQ', TYPE, DIM, J, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL NAMEST (DDNAME, J, DDSTR, DDSLEN)
         CALL TABGET (TBEDIT, 'TBLTYPE', TYPE, DIM, DUMMY, DDTYPE, IERR)
         IF (IERR.NE.0) GO TO 980
         DDSLEN = DDSLEN + 4
         DDSTR(DDSLEN:) = DDTYPE // ' VERS'
         DDSLEN = DDSLEN + 8
         WRITE (MSGBUF,1030) VERS
         CALL CHTRIM (MSGBUF, 8, MSGBUF, J)
         DDSTR(DDSLEN:) = MSGBUF(:J)
         DDSLEN = DDSLEN + 3 + J
         DDSTR(DDSLEN:) = 'FQID'
         DDSLEN = DDSLEN + 5
         WRITE (MSGBUF,1030) FRQSEL
         CALL CHTRIM (MSGBUF, 8, MSGBUF, J)
         DDSTR(DDSLEN:) = MSGBUF(:J)
         DDSLEN = DDSLEN + J - 1
C                                       close and reopen for update
         CALL TABCLO (TBEDIT, IERR)
         IF (IERR.NE.0) GO TO 980
         STATUS = 'WRIT'
         CALL TABOPN (TBEDIT, STATUS, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       basic parameters
         TIMEM = 0
         TIME1 = 1
         TIME2 = 0
         ALLIN = .FALSE.
         PIFNOW = -1
         MAXREC = 0
         POLNOW = 1
         IF (CROWDP) POLNOW = 0
         LTYPE = 1
         LTYPE2 = 0
         IF (DOCOMP) LTYPE2 = 2
         IFNOW = BIF
         IF (CROWDI) IFNOW = 0
C                                       no default range
         CALL RFILL (8, 0.0, DPIXR)
C                                       Open terminal for conversation
         CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *      IERR)
         IF (IERR.NE.0) THEN
            TTY(2) = 0
            WRITE (MSGTXT,1035) IERR
            CALL MSGWRT (8)
            PROBLM = 'The terminal'
            GO TO 980
            END IF
         TTY(2) = MAX (1, TTY(2))
C                                       Graphics: menu, menu back,
C                                       editing, editdata, flagged data,
C                                       extra data, full data
C                                       2 3 4 1 6 5 7
         GRSEL(1) = 6
         GRSEL(2) = 3
         GRSEL(3) = 4
         GRSEL(4) = 1
         GRSEL(5) = 5
         GRSEL(6) = 2
         GRSEL(7) = 0
C                                       learn about TV
         PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         MINWIN(1) = (36 + CEDG) * CSIZE(1) + 300
         MINWIN(2) = 37 * CSIZE(2) + 150
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
C                                       off all TV channels
         CALL TVDOPR (TVNAME, 'HOLD', I, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDOPR (TVNAME, 'INIT', I, IERR)
         IF (IERR.NE.0) GO TO 980
         I = 1
         CALL TVDOPR (TVNAME, 'TVOF', I, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       clear and off all graphics
         DO 35 I = 1,NGRPH-1
            CALL TVDOPR (TVNAME, 'GRON', I, IERR)
            IF (IERR.NE.0) GO TO 980
 35         CONTINUE
         CALL TVDFUN (TVNAME, 'OFFZ', 0, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDZOM (TVNAME, 'READ', NOZOOM, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (DO3COL) THEN
            CALL ED3CIN (IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Get some data
         WASFLG = -1
         PROBLM = TBEDIT
         CALL EDGTSY (IERR)
         IF (IERR.NE.0) GO TO 980
         APOPEN = .TRUE.
         IF (POLMAX.LE.1) THEN
            CROWDP = .FALSE.
            POLNOW = 1
            END IF
         IF (.NOT.ALLIN) THEN
            CROWDI = .FALSE.
            IFNOW = BIF
            END IF
C                                       get initial antennas to plot
         NUMPLT = 0
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'ANTS2USE', TYPE, DIM, AVAL, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            CALL FILL (50, 0, AVAL)
            IERR = 0
            END IF
         DO 40 I = 1,NPLT
            IF ((AVAL(I).GT.0) .AND. (AVAL(I).LE.ANTMAX) .AND.
     *         (EDAFND(AVAL(I), ANTMLX,ANTEN)) .AND.
     *         (MSAMPS(AVAL(I)).GT.0)) THEN
               NUMPLT = NUMPLT + 1
               PLTAN(NUMPLT) = AVAL(I)
               END IF
 40         CONTINUE
         IF (NUMPLT.LE.0) THEN
            DO 45 I = 1,ANTMLX
               IF (MSAMPS(ANTEN(I)).GT.0) THEN
                  NUMPLT = NUMPLT + 1
                  IF (NUMPLT.LE.NPLT) PLTAN(NUMPLT) = ANTEN(I)
                  END IF
 45            CONTINUE
            NUMPLT = MIN (NUMPLT, 3)
            END IF
         ALLANT = PLTAN(1)
C                                       plot the data
         PROBLM = TVNAME
         CALL EDPLOT (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       set parameters for menu
         TVCOL = 2
         IF (UVFLAG) THEN
            TVROWS(1) = NCOLB
            DO 50 I = 1,NCOLB
               CHOICS(I) = LISTB(I)
 50            CONTINUE
         ELSE
            TVROWS(1) = NCOL1
            DO 55 I = 1,NCOL1
               CHOICS(I) = LIST1(I)
 55            CONTINUE
            END IF
         J = TVROWS(1)
         IF (POLMAX*(EIF-BIF+1).GT.2) THEN
            J = J + 1
            CHOICS(J) = LIST2(1)
            END IF
         IF (POLMAX.GT.1) THEN
            J = J + 1
            CHOICS(J) = LIST2(2)
            J = J + 1
            CHOICS(J) = LIST2(3)
            END IF
         IF (EIF.GT.BIF) THEN
            J = J + 1
            CHOICS(J) = LIST2(4)
            IF (DIMIF.GT.1) THEN
               J = J + 1
               CHOICS(J) = LIST2(5)
               END IF
            END IF
         CHSHO2 = 0
         CHSHOW = 0
         DO 60 I = 6,NCOL2
            J = J + 1
            CHOICS(J) = LIST2(I)
            IF (CHOICS(J).EQ.'SHOW PSUM') CHSHOW = J
            IF (CHOICS(J)(:10).EQ.'SHOW ALSO ') THEN
               IF (CHSHO2.LE.0) CHSHO2 = J
               IF (.NOT.DOCOMP) J = J - 1
               END IF
 60         CONTINUE
         TVROWS(2) = J - TVROWS(1)
         IF (UVFLAG) THEN
            ISHELP = 'EDISY'
         ELSE
            ISHELP = 'EDISS'
            END IF
         TIMLIM = 0
         IG(1) = GRSEL(1)
         IG(2) = GRSEL(2)
         CALL FILL (NXPRT, 1, OPTOKX)
         CALL FILL (NXPRTB, 1, OPTOKB)
         OPTOKX(NXPRT-2) = -1
         OPTOKB(NXPRTB-2) = -1
         IF ((POLMAX.LE.1) .AND. (EIF.EQ.BIF)) THEN
            OPTOKX(NXPRT-5) = -1
            OPTOKB(NXPRTB-5) = -1
            END IF
C                                       LOOP POINT
C                                       window still big enough?
 100     PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
         I = LTVWND(4) - TOPLOT + 5 * CSIZE(2) - 1
         IF (NEWPLT) THEN
            CALL TVDOKA (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            GRSTAT(IG(1)) = 1
            CALL TVDRST (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            ILAST = I
            END IF
         IF (PREXIS) THEN
            CHOICE = 'REDO FLAGS'
         ELSE IF ((EXPERT) .AND. (UVFLAG)) THEN
            CALL TVDXPR (TVNAME, -1, TVCOL, TVROWS, IG, I, CHOICS,
     *         LEAVE, TTY, NXPRTB, CHOICB, SHORTB, ALPHAB, OPTOKB,
     *         CHOICE, IERR)
            PROBLM = 'The terminal'
            IF (IERR.NE.0) GO TO 980
            J = TVROWS(1) + TVROWS(2)
            CALL EDCFND (CHOICE, J, CHOICS, CHS)
         ELSE IF (EXPERT) THEN
            CALL TVDXPR (TVNAME, -1, TVCOL, TVROWS, IG, I, CHOICS,
     *         LEAVE, TTY, NXPRT, CHOICX, SHORTX, ALPHAX, OPTOKX,
     *         CHOICE, IERR)
            PROBLM = 'The terminal'
            IF (IERR.NE.0) GO TO 980
            J = TVROWS(1) + TVROWS(2)
            CALL EDCFND (CHOICE, J, CHOICS, CHS)
         ELSE
            TITLE = ' '
            NTITLE = 0
            SIDSEP = 7
            CALL TVDMEN (TVNAME, -1, TVCOL, TVROWS, IG, I, SIDSEP,
     *         ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHS,
     *         BUTTON, IERR)
            IF (IERR.NE.0) GO TO 980
            CHOICE = CHOICS(CHS)
            END IF
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
         IF (NEWPLT) ILAST = 0
C                                       Do something:
         IF (LOADIT) DOPLOT = .FALSE.
C                                       load/hold
         IF ((CHOICE.EQ.'HOLD TV LOAD') .OR.
     *      (CHOICE.EQ.'DO TV LOAD')) THEN
            IF (LOADIT) THEN
               CHOICS(CHS) = 'DO TV LOAD'
            ELSE
               CHOICS(CHS) = 'HOLD TV LOAD'
               END IF
            ILAST = 0
            LOADIT = .NOT.LOADIT
C                                       flag interaction
         ELSE IF (CHOICE(:5).EQ.'FLAG ') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (CHOICE.EQ.'FLAG TIME') THEN
               CALL EDFLTI (IERR)
            ELSE IF (CHOICE.EQ.'FLAG TIME RANGE') THEN
               CALL EDFLTR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG BELOW') THEN
               CALL EDFLFB (IERR)
            ELSE IF (CHOICE.EQ.'FLAG ABOVE') THEN
               CALL EDFLFA (IERR)
            ELSE IF (CHOICE.EQ.'FLAG AREA') THEN
               CALL EDFLAR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG POINT') THEN
               CALL EDFLPT (IERR)
            ELSE IF (CHOICE.EQ.'FLAG QUICKLY') THEN
               CALL EDFLQU (IERR)
            ELSE
               CALL EDSTUB ('FLAG', CHOICE, OBJECT)
               END IF
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       list FC table
         ELSE IF (CHOICE.EQ.'LIST FLAGS') THEN
            CALL EDFCLI (IERR)
C                                       Undo flags in FC table
         ELSE IF (CHOICE.EQ.'UNDO FLAGS') THEN
            IF (FLGNMX.LE.1) THEN
               IFLN(1) = FLGNMX
               IFLN(2) = FLGNMX
               IERR = 0
            ELSE
               WRITE (MSGBUF,1100) FLGNMX
               CALL INQINT (TTY, MSGBUF, 2, IFLN, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
               IF (IFLN(2).LT.IFLN(1)) IFLN(2) = IFLN(1)
               END IF
            IF ((IERR.EQ.0) .AND. (IFLN(1).GT.0)) THEN
               PROBLM = TVNAME
               CALL EDFCUN (IFLN, IERR)
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Redo flags in FC table
         ELSE IF (CHOICE.EQ.'REDO FLAGS') THEN
            CALL EDFCDO (IERR)
            PREXIS = .FALSE.
C                                       interp at flag point
         ELSE IF (CHOICE.EQ.'SMOOTH TIME RNG') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL EDSMTR (IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Re-read at flag point
         ELSE IF (CHOICE.EQ.'REREAD TIME RNG') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL EDRRTR (IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Unflag
         ELSE IF (CHOICE(:8).EQ.'RESTORE ') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (CHOICE.EQ.'RESTORE TIME') THEN
               CALL EDRSTI (IERR)
            ELSE IF (CHOICE.EQ.'RESTORE AREA') THEN
               CALL EDRSAR (IERR)
            ELSE
               CALL EDSTUB ('RESTORE', CHOICE, OBJECT)
               END IF
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'ENTER ANTENNA') THEN
            MSGBUF = 'Enter number of antenna to be edited'
            CALL INQINT (TTY, MSGBUF, 1, I, IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.LT.0) THEN
               I = PLTAN(1)
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
            IF ((I.LE.0) .OR. (I.GT.ANTMAX)) THEN
               WRITE (MSGTXT,1210) 'ANTENNA', I, 1, ANTMAX
               CALL MSGWRT (6)
            ELSE IF ((EDAFND (I, ANTMLX, ANTEN)) .AND. (MSAMPS(I).GT.0))
     *         THEN
               IF (I.NE.PLTAN(1)) THEN
                  LT1 = 0
                  LT2 = 0
                  DO 105 J = 2,NUMPLT
                     IF (PLTAN(J).EQ.I) LT1 = J
                     IF (PLTAN(J).EQ.PLTAN(1)) LT2 = J
 105                 CONTINUE
                  IF (LT1.GT.0) THEN
                     IF (LT2.EQ.0) THEN
                        PLTAN(LT1) = PLTAN(1)
                     ELSE
                        CALL COPY (NUMPLT-LT1, PLTAN(LT1+1), PLTAN(LT1))
                        NUMPLT = NUMPLT - 1
                        END IF
                     END IF
                  PLTAN(1) = I
                  IF (ALLANT.GT.0) ALLANT = PLTAN(1)
                  DOPLOT = .TRUE.
                  PNDING = .TRUE.
                  END IF
            ELSE
               WRITE (MSGTXT,1211) I
               CALL MSGWRT (6)
               END IF
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'NEXT ANTENNA') THEN
            I = PLTAN(1)
 110        I = I + 1
               IF (I.GT.ANTMAX) I = 1
               IF (MSAMPS(I).LE.0) GO TO 110
               IF (.NOT.(EDAFND (I, ANTMLX, ANTEN))) GO TO 110
            IF (I.NE.PLTAN(1)) THEN
               LT1 = 0
               LT2 = 1
               DO 115 J = 2,NUMPLT
 111              IF (PLTAN(J).EQ.I) THEN
                     CALL COPY (NUMPLT-J, PLTAN(J+1), PLTAN(J))
                     PLTAN(NUMPLT) = 0
                     GO TO 111
                     END IF
                  LT1 = MAX (LT1, PLTAN(J))
                  IF (PLTAN(J).GT.0) LT2 = J
 115              CONTINUE
               PLTAN(1) = I
               IF (ALLANT.GT.0) ALLANT = PLTAN(1)
C                                       add to other ants
 120           IF (LT2.LT.NUMPLT) THEN
                  I = LT1
                  IF (I.EQ.0) I = PLTAN(1)
                  DO 125 K = 1,ANTMAX
                     I = I + 1
                     IF (I.GT.ANTMAX) I = 1
C                                       good antenna - used already?
                     IF ((EDAFND (I, ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(I).GT.0)) THEN
                        DO 121 J = 1,LT2
                           IF (I.EQ.PLTAN(J)) GO TO 125
 121                       CONTINUE
C                                       found a new one
                        LT2 = LT2 + 1
                        PLTAN(LT2) = I
                        LT1 = MAX (LT1, I)
                        GO TO 120
                        END IF
 125                 CONTINUE
                  NUMPLT = LT2
                  END IF
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Other ants
         ELSE IF (CHOICE.EQ.'ENTER OTHER ANT') THEN
            I = NPLT - 1
            WRITE (MSGBUF,1220) I
            CALL INQINT (TTY, MSGBUF, I, IVAL(2), IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.EQ.0) THEN
               K = NUMPLT
               NUMPLT = 1
               DO 135 I = 2,NPLT
                  IF ((IVAL(I).GT.0) .AND. (IVAL(I).LE.ANTMAX)) THEN
                     IF ((EDAFND (IVAL(I), ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(IVAL(I)).GT.0)) THEN
                        NUMPLT = NUMPLT + 1
                        PLTAN(NUMPLT) = IVAL(I)
                        END IF
                     END IF
 135              CONTINUE
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               IF (K.NE.NUMPLT) THEN
                  CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
                  IF (IERR.NE.0) GO TO 980
                  NEWPLT = .TRUE.
                  ILAST = 0
                  END IF
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Select IF
         ELSE IF (CHOICE.EQ.'ENTER IF') THEN
            IF ((EIF.EQ.BIF+1) .AND. (.NOT.CROWDI)) THEN
               I = 2 * BIF + 1 - IFNOW
               IERR = 0
            ELSE IF ((EIF.EQ.BIF+1) .AND. (CROWDI)) THEN
               IF (IFNOW.EQ.0) THEN
                  I = BIF
               ELSE IF (IFNOW.EQ.EIF) THEN
                  I = 0
               ELSE
                  I = EIF
                  END IF
               IERR = 0
            ELSE
               MSGBUF = 'Enter number of IF to be plotted and edited'
               CALL INQINT (TTY, MSGBUF, 1, I, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
               IF (IERR.LT.0) THEN
                  I = IFNOW
                  MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                  CALL MSGWRT (6)
                  IERR = 0
                  END IF
               END IF
            IF (((I.LT.BIF) .OR. (I.GT.EIF)) .AND. ((I.NE.0) .OR.
     *         (.NOT.CROWDI))) THEN
               WRITE (MSGTXT,1210) 'IF', I, BIF, EIF
               CALL MSGWRT (6)
            ELSE IF (I.NE.IFNOW) THEN
               PNDING = .TRUE.
               PIFNOW = IFNOW
               IFNOW = I
               IF ((ALLIF.EQ.0) .AND. ((IFNOW.LT.FGIFRS(1)) .OR.
     *            (IFNOW.GT.FGIFRS(2)))) THEN
                  WRITE (MSGTXT,1221) IFNOW, FGIFRS
                  CALL MSGWRT (7)
                  END IF
               IF (.NOT.ALLIN) THEN
                  PROBLM = TBEDIT
                  CALL EDGTSY (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               DOPLOT = .TRUE.
               END IF
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Smooth parameters
         ELSE IF (CHOICE.EQ.'ENTER SMOOTH PS') THEN
            MSGBUF = 'Enter smooth type (2PT BOX MWF 2PTH GAUS LINE '
     *         // 'EXP) left justified'
 136        CALL INQSTR (TTY, MSGBUF, 4, STATUS, IERR)
            IF (IERR.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 136
               END IF
            CALL CHLTOU (4, STATUS)
            IF ((IERR.EQ.0) .AND. ((STATUS.EQ.'2PT') .OR.
     *         (STATUS.EQ.'BOX') .OR. (STATUS.EQ.'MWF') .OR.
     *         (STATUS.EQ.'2PTH') .OR. (STATUS.EQ.'GAUS') .OR.
     *         (STATUS.EQ.'LINE') .OR. (STATUS.EQ.'EXP '))) THEN
               SMTYPE = STATUS
               MSGBUF = 'Enter smooth box full width in seconds'
               CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
               IF ((IERR.EQ.0) .AND. (I.GT.0) .AND. (I.LT.86400)) THEN
                  SMTIME(2) = DVAL(1)
                  IF ((SMTYPE.EQ.'GAUS') .OR. (SMTYPE.EQ.'LINE') .OR.
     *               (SMTYPE.EQ.'EXP ')) THEN
                     MSGBUF = 'Enter full width half max in seconds'
                     CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
                     IF ((IERR.EQ.0) .AND. (I.GT.0) .AND. (I.LT.86400))
     *                  THEN
                        SMTIME(3) = DVAL(1)
                        CALL EDPLST (GRSEL(6), IERR)
                     ELSE
                        MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                        CALL MSGWRT (6)
                        END IF
                     END IF
               ELSE
                  MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                  CALL MSGWRT (6)
                  END IF
            ELSE IF ((IERR.NE.0) .OR. (STATUS.NE.' ')) THEN
               MSGTXT = 'THERE WAS A PROBLEM: SMTYPE KEPT AS ' // SMTYPE
               CALL MSGWRT (6)
               END IF
            IERR = MAX (IERR,0)
C                                       scan length for diff
         ELSE IF (CHOICE.EQ.'SET SCAN LENGTH') THEN
            MSGBUF = 'Enter ''scan'' length in seconds'
            CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
            IF (IERR.GT.0) GO TO 980
C                                       recompute the differences
            IF ((IERR.EQ.0) .AND. (DVAL(1).GT.0) .AND.
     *         (DVAL(1).LT.86400) .AND. (DVAL(1).NE.SMTIME(1))) THEN
               SMTIME(1) = DVAL(1)
               FLAGED = .TRUE.
               CALL EDPLST (GRSEL(6), IERR)
               DOPLOT = (LTYPE.EQ.3) .OR. (DOCOMP .AND. (LTYPE2.EQ.3))
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Set reason
         ELSE IF (CHOICE.EQ.'SET REASON') THEN
            MSGBUF = 'Enter new reason, left justified'
 137        CALL INQSTR (TTY, MSGBUF, 24, REAZON, IERR)
            IF (IERR.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 137
               END IF
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF ((IERR.EQ.0) .AND. (REAZON.NE.'-')) THEN
               REASON = REAZON
               I = ITRIM (REASON)
               I = MAX (1, I)
               MSGTXT = 'Using reason = ''' // REASON(:I) // ''''
               CALL MSGWRT (2)
               END IF
            IERR = MAX (0, IERR)
C                                       Set ranges
         ELSE IF ((CHOICE(:7).EQ.'ENTER T') .OR.
     *      (CHOICE(:7).EQ.'ENTER P')) THEN
            IF (CHOICE.EQ.'ENTER PDIF RNG') I = 1
            IF (CHOICE.EQ.'ENTER PSUM RNG') I = 2
            IF (CHOICE.EQ.'ENTER PGAIN RNG') I = 3
            IF (CHOICE.EQ.'ENTER TSYS RNG') I = 4
            IF (CHOICE.EQ.'ENTER PD/P RNG') I = 5
            IF (CHOICE.EQ.'ENTER PS/P RNG') I = 6
            IF (CHOICE.EQ.'ENTER PG/P RNG') I = 7
            IF (CHOICE.EQ.'ENTER TS/T RNG') I = 8
            J = ITRIM (DTYPE(I))
            MSGBUF = 'Enter ' // DTYPE(I)(:J) // ' display range in ' //
     *         DUNITS(I)
            CALL INQFLT (TTY, MSGBUF, 2, DVAL, IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.EQ.0) THEN
               DPIXR(1,I) = DVAL(1) * DPLSCL(I)
               DPIXR(2,I) = DVAL(2) * DPLSCL(I)
               IF ((I.EQ.LTYPE) .OR. (DOCOMP .AND. (I.EQ.LTYPE2))) THEN
                  DOPLOT = .TRUE.
                  PNDING = .TRUE.
                  END IF
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Switch polarization
         ELSE IF (CHOICE.EQ.'SWITCH POLARIZ') THEN
            IF (CROWDP) THEN
               POLNOW = MOD (POLNOW+1, 3)
            ELSE
               POLNOW = 3 - POLNOW
               END IF
            DOPLOT = .TRUE.
            PNDING = .TRUE.
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Next Correlator
         ELSE IF (CHOICE.EQ.'NEXT POL/IF') THEN
            PNDING = .TRUE.
            IF (POLNOW.LT.POLMAX) THEN
               POLNOW = POLNOW + 1
               IFNOW = MAX (BIF, IFNOW)
            ELSE
               POLNOW = 1
               IF (CROWDP) POLNOW = 0
               PIFNOW = IFNOW
               IFNOW = MAX (BIF, IFNOW+1)
               IF (IFNOW.GT.EIF) THEN
                  IFNOW = BIF
                  IF (CROWDI) IFNOW = 0
                  END IF
               PROBLM = TBEDIT
               IF (.NOT.ALLIN) THEN
                  CALL EDGTSY (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               END IF
            DOPLOT = .TRUE.
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Switch all ant
         ELSE IF (CHOICE.EQ.'SWITCH ALL ANT') THEN
            IF (ALLANT.GT.0) THEN
               ALLANT = 0
            ELSE
               ALLANT = PLTAN(1)
               END IF
            CALL EDPLST (GRSEL(6), IERR)
C                                       Switch all pol
         ELSE IF (CHOICE.EQ.'SWITCH ALL POL') THEN
            IF (POLMAX.GT.1) THEN
               ALLPOL = .NOT.ALLPOL
               CALL EDPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all IF
         ELSE IF (CHOICE.EQ.'SWITCH ALL IF') THEN
            IF (DIMIF.GT.1) THEN
               IF ((DIMIF.GT.2) .AND. (ALLIN)) THEN
                  ALLIF = ALLIF + 1
                  IF (ALLIF.EQ.2) THEN
                     ALLIF = -1
                  ELSE IF (ALLIF.EQ.0) THEN
                     MSGBUF = 'Enter IF number range to flag'
                     CALL INQINT (TTY, MSGBUF, 2, ITEMP, IERR)
                     PROBLM = 'The terminal'
                     IF (IERR.GT.0) GO TO 980
                     IF (IERR.EQ.0) THEN
                        IF ((ITEMP(1).EQ.0) .AND. (ITEMP(2).EQ.0)) THEN
                           ALLIF = -1
                        ELSE IF ((ITEMP(2).LT.ITEMP(1)) .OR.
     *                     (ITEMP(1).LT.BIF) .OR. (ITEMP(1).GT.EIF) .OR.
     *                     (ITEMP(2).LT.BIF) .OR. (ITEMP(2).GT.EIF))
     *                     THEN
                           IERR = -1
                        ELSE
                           FGIFRS(1) = ITEMP(1)
                           FGIFRS(2) = ITEMP(2)
                           IF ((IFNOW.LT.FGIFRS(1)) .OR.
     *                        (IFNOW.GT.FGIFRS(2))) THEN
                              WRITE (MSGTXT,1221) IFNOW, FGIFRS
                              CALL MSGWRT (7)
                              END IF
                           END IF
                        END IF
                     IF (IERR.LT.0) THEN
                        I = IFNOW
                        MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                        CALL MSGWRT (6)
                        IERR = 0
                        END IF
                     END IF
               ELSE
                  ALLIF = -ALLIF
                  END IF
               CALL EDPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all times
         ELSE IF (CHOICE.EQ.'SWITCH ALL TIME') THEN
            ALLTIM = .NOT.ALLTIM
            CALL EDPLST (GRSEL(6), IERR)
C                                       Switch all source
         ELSE IF (CHOICE.EQ.'SWITCH ALL SOURC') THEN
            ALLSOR = .NOT.ALLSOR
            CALL EDPLST (GRSEL(6), IERR)
C                                       TV zoom
         ELSE IF (CHOICE.EQ.'TV ZOOM') THEN
            CALL OTVZOM (TVNAME, IERR)
            CALL TVDZOM (TVNAME, 'READ', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Off zoom
         ELSE IF (CHOICE.EQ.'OFF ZOOM') THEN
            CALL OTVOFZ (TVNAME, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'READ', SVZOOM, IERR)
C                                       expert mode switch
         ELSE IF (CHOICE.EQ.'USE EXPERT MODE') THEN
            EXPERT = .NOT.EXPERT
C                                       full plot
         ELSE IF (CHOICE.EQ.'PLOT ALL TIMES') THEN
            IF ((TIME1.GT.1) .OR. (TIME2.LT.TIMEM)) THEN
               TIME1 = 1
               TIME2 = TIMEM-1
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Select frame
         ELSE IF (CHOICE.EQ.'SELECT FRAME') THEN
            IF ((TIME1.GT.1) .OR. (TIME2.LT.TIMEM-1) .OR. PNDING) THEN
               TIME1 = 1
               TIME2 = TIMEM-1
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL EDFRAM (IERR)
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Next frame
         ELSE IF (CHOICE.EQ.'NEXT FRAME') THEN
            LT2 = MIN (2*TIME2-TIME1, TIMEM-1)
            LT1 = LT2 - TIME2 + TIME1
            IF ((TIME1.NE.LT1) .OR. (TIME2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               TIME1 = LT1
               TIME2 = LT2
               END IF
C                                       Previous frame
         ELSE IF (CHOICE.EQ.'PREVIOUS FRAME') THEN
            LT1 = MAX (2*TIME1-TIME2, 1)
            LT2 = LT1 + TIME2 - TIME1
            IF ((TIME1.NE.LT1) .OR. (TIME2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               TIME1 = LT1
               TIME2 = LT2
               END IF
C                                       Show also type
         ELSE IF (CHOICE(:10).EQ.'SHOW ALSO ') THEN
            IF (DOCOMP) THEN
               J = LTYPE2
               IF (CHOICE(11:).EQ.'PDIF') LTYPE2 = 1
               IF (CHOICE(11:).EQ.'PSUM') LTYPE2 = 2
               IF (CHOICE(11:).EQ.'PGAIN') LTYPE2 = 3
               IF (CHOICE(11:).EQ.'TSYS') LTYPE2 = 4
               IF (CHOICE(11:).EQ.'DPDIF') LTYPE2 = 5
               IF (CHOICE(11:).EQ.'DPSUM') LTYPE2 = 6
               IF (CHOICE(11:).EQ.'DPGAIN') LTYPE2 = 7
               IF (CHOICE(11:).EQ.'DTSYS') LTYPE2 = 8
               K = CHSHO2
               DO 225 I = 1,8
                  IF ((I.NE.LTYPE) .AND. (I.NE.LTYPE2)) THEN
                     CHOICS(K) = SHOW2(I)
                     K = K + 1
                     END IF
 225              CONTINUE
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Show a type
         ELSE IF (CHOICE(:5).EQ.'SHOW ') THEN
            I = LTYPE
            J = LTYPE2
            IF (CHOICE(6:).EQ.'PDIF') LTYPE = 1
            IF (CHOICE(6:).EQ.'PSUM') LTYPE = 2
            IF (CHOICE(6:).EQ.'PGAIN') LTYPE = 3
            IF (CHOICE(6:).EQ.'TSYS') LTYPE = 4
            IF (CHOICE(6:).EQ.'PDIF/<P>-1') LTYPE = 5
            IF (CHOICE(6:).EQ.'PSUM/<P>-1') LTYPE = 6
            IF (CHOICE(6:).EQ.'PGAIN/<P>-1') LTYPE = 7
            IF (CHOICE(6:).EQ.'TSYS/<T>-1') LTYPE = 8
            IF (DOCOMP) THEN
               IF (LTYPE2.EQ.LTYPE) THEN
                  IF (LTYPE.GT.3) THEN
                     LTYPE2 = LTYPE + 1
                     IF (LTYPE2.EQ.8) LTYPE2 = 5
                  ELSE
                     LTYPE2 = MOD (LTYPE, 4) + 1
                     END IF
                  END IF
               END IF
            IF ((I.NE.LTYPE) .OR. (J.NE.LTYPE2)) THEN
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               IF (UVFLAG) THEN
                  IF (I.LE.4) OPTOKB(NXPRTB-4+I) = 1
                  IF (LTYPE.LE.4) OPTOKB(NXPRTB-4+LTYPE) = -1
               ELSE
                  IF (I.LE.4) OPTOKX(NXPRT-4+I) = 1
                  IF (LTYPE.LE.4) OPTOKX(NXPRT-4+LTYPE) = -1
                  END IF
               K = CHSHOW
               DO 230 I = 1,8
                  IF (I.NE.LTYPE) THEN
                     CHOICS(K) = SHOW(I)
                     K = K + 1
                     END IF
 230              CONTINUE
               IF (DOCOMP) THEN
                  K = CHSHO2
                  DO 235 I = 1,8
                     IF ((I.NE.LTYPE) .AND. (I.NE.LTYPE2)) THEN
                        CHOICS(K) = SHOW2(I)
                        K = K + 1
                        END IF
 235                 CONTINUE
                  END IF
               END IF
C                                       REPLOT
         ELSE IF (CHOICE.EQ.'REPLOT') THEN
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       EXIT
         ELSE IF (CHOICE.EQ.'EXIT') THEN
            IERR = 0
            GO TO 995
C                                       ABORT
         ELSE IF (CHOICE.EQ.'ABORT') THEN
            IERR = -1
            GO TO 995
            END IF
         IF ((IERR.LE.0) .AND. (DOPLOT)) THEN
            IF (LOADIT) THEN
               CALL EDPLOT (IERR)
            ELSE IF (PNDING) THEN
               CALL EDPLST (GRSEL(6), IERR)
               END IF
            PROBLM = TVNAME
            END IF
         IF (IERR.GT.0) GO TO 980
         GO TO 100
C                                       apply result
      ELSE IF (OPCODE.EQ.'APPL') THEN
         PROBLM = TBEDIT
         PIFNOW = IFNOW
         IFNOW = -1
         CALL EDGTSY (IERR)
         IF (IERR.NE.0) GO TO 980
         IF (UVFLAG) THEN
            CALL EDFCAP (UVMAST, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TABZAP (FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'EDGTSY', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 800 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 800        CONTINUE
         IF (DO3COL) THEN
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+1, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+2, IERR)
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       do not apply, don't destroy
      ELSE IF ((OPCODE.EQ.'ABOR') .OR. (OPCODE.EQ.'KILL')) THEN
         PROBLM = TBEDIT
         IF ((UVFLAG) .AND. (FCREAT) .AND. (OPCODE.NE.'KILL')) THEN
            CALL TABZAP (FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'EDGTSY', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 810 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 810        CONTINUE
         IF (DO3COL) THEN
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+1, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+2, IERR)
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       ??
      ELSE
         IERR = 2
         CALL EDSTUB ('EDITSY', OPCODE, OBJECT)
         END IF
      GO TO 995
C                                       error
 980  MSGTXT = 'EDITSY: ERROR WITH OBJECT ' // PROBLM
      CALL MSGWRT (7)
C
 995  IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NO_POL = ',I3,' LIMITED HERE TO 2')
 1030 FORMAT (I8)
 1035 FORMAT ('ERROR',I6,' OPENING THE TERMINAL')
 1100 FORMAT ('Enter flag command number range to be undone: 1 to',I5)
 1210 FORMAT (A,I5,' OUT OF RANGE',I4,' to',I4)
 1211 FORMAT ('ANTENNA',I4,' NOT IN LIST')
 1220 FORMAT ('Enter ',I2,' antenna numbers to display')
 1221 FORMAT ('WARNING: CURRENT IF',I3,' OUTSIDE FLAG RANGE',2I3)
      END
      SUBROUTINE EDITUV (OPCODE, OBJECT, IERR)
C-----------------------------------------------------------------------
C   edit UV-data with the data themselves
C   Inputs:
C      OPCODE   C*4    Operation:
C                         'INIT' start a new edit session
C                         'ABOR' close session, delete flag info
C                         'KILL' close session, save FC table
C                         'APPL' close session, put flag info in master
C      OBJECT   C*(*)  Open Edit object
C   Output:
C      IERR     I      Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C                         < 0 => dies by the users' hands
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OBJECT*(*)
      INTEGER   IERR
C
      INTEGER   NCOL1, NCOL2, NCOLS, NXPRT
      PARAMETER (NCOL1 = 22, NCOL2 = 22, NCOLS = NCOL1+NCOL2)
      PARAMETER (NXPRT = 16)
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'EDIFCPS.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER   TYPE, DIM(7), I, J, K, TVCOL, TVROWS(2), TIMLIM, CHS,
     *   TVWND(4), MINWIN(2), VERS, IG(8), BUTTON, DUMMY, TTY(2), ITRIM,
     *   IVAL(NPLT), NOZOOM(3), SVZOOM(3), ILAST, LT1, LT2, TVSTAT(16),
     *   GRSTAT(8), PLTANS(NPLT), JERR, NAPLT, CHSHOW, CHSHO2, NTITLE,
     *   OPTOKX(NXPRT), MSGSAV, IFLN(2), AVAL(50), ITEMP(2), SIDSEP
      REAL      T(8), RDUM
      DOUBLE PRECISION DVAL(2)
      LOGICAL   EDAFND, DOPLOT, LOADIT, LEAVE(NCOLS)
      CHARACTER PROBLM*32, CDUMMY*1, CHOICS(NCOLS)*16, ISHELP*6,
     *   LIST1(NCOL1)*16, LIST2(NCOL2)*16, MSGBUF*72, DDNAME*18,
     *   INKT(4)*8, INEXT*2, SORD*4, STOKES*4, REAZON*24, SHOW(3)*16,
     *   SHOW2(3)*16, CHOICE*16, CHOICX(NXPRT)*16, SHORTX(NXPRT)*4,
     *   ALPHAX(NXPRT)*1, TITLE*8
      DATA INKT /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
      DATA LIST1 /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'ENTER AMPL RNG', 'ENTER PHASE RNG', 'ENTER DAMP RNG',
     *   'PLOT ERROR BARS',  'SET SCAN LENGTH', 'LIST FLAGS',
     *   'UNDO FLAGS', 'REDO FLAGS', 'SET REASON', 'USE EXPERT MODE',
     *   'HOLD TV LOAD', 'REPLOT', ' ', 'EXIT', 'ABORT'/
      DATA LIST2 /'NEXT POL/IF', 'SWITCH POLARIZ', 'SWITCH ALL POL',
     *   'ENTER IF', 'SWITCH ALL IF', 'SWITCH ALL TIME',
     *   'ROTATE ALL ANT', 'SWITCH ALL SOURC', 'ENTER ANTENNA',
     *   'ENTER OTHER ANT', 'NEXT BASELINE', 'NEXT ANTENNA',
     *   'PLOT ALL TIMES', 'SELECT FRAME', 'NEXT FRAME',
     *   'PREVIOUS FRAME', 'SHOW PHASE', 'SHOW DIFF AMPL',
     *   'SHOW ALSO DAMP', 'TV ZOOM', 'OFF ZOOM', '2ND UV OFF'/
      DATA SHOW, SHOW2  /'SHOW AMPLITUDE', 'SHOW PHASE',
     *   'SHOW DIFF AMPL', 'SHOW ALSO AMPL', 'SHOW ALSO PHASE',
     *   'SHOW ALSO DAMP'/
      DATA CHOICX /'FLAG TIME', 'FLAG TIME RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'LIST FLAGS', 'UNDO FLAGS', 'NEXT BASELINE', 'NEXT POL/IF',
     *   'NEXT ANTENNA', 'SELECT FRAME','SHOW AMPLITUDE', 'SHOW PHASE',
     *   'SHOW DIFF AMPL'/
      DATA SHORTX /'Time', 'Rang', 'Belo', 'Abov', 'arEa', 'pOin',
     *   'Quik', 'List', 'Undo', 'baSl', 'nCor', 'Nant', 'Fram', 'aMpl',
     *   'Phas', 'aDif'/
      DATA ALPHAX /'T', 'R', 'B', 'A', 'E', 'O', 'Q', 'L', 'U', 'S',
     *   'C', 'N', 'F', 'M', 'P', 'D'/
      DATA LEAVE /NCOLS*.TRUE./
C-----------------------------------------------------------------------
      TTY(1) = 5
      TTY(2) = 0
      WASACR = .FALSE.
      WASXCR = .FALSE.
      ALLSOR = .FALSE.
C                                       plot scale ranges
      CALL RFILL (8, 1.0, RSCALE)
C                                       Init the functions
      IF (OPCODE.EQ.'INIT') THEN
         FLAGED = .TRUE.
         APOPEN = .FALSE.
         FCREAT = .FALSE.
         DOERRB = .FALSE.
         GRSEL(8) = 0
         DO 5 I = 1,MAXIF
            IFWASN(I) = .FALSE.
 5          CONTINUE
         FLGMSG = .TRUE.
         SCMAXS = SCMAXX
         ILAST = 0
         NEWPLT = .TRUE.
         DTYPE(1) = 'Amplitude'
         DTYPE(2) = 'Phase'
         DTYPE(3) = 'Diff Ampl'
         DTYPE(4) = '????????'
         DUNITS(1) = 'Jy'
         DUNITS(2) = 'Degrees'
         DUNITS(3) = 'Jy'
         DUNITS(4) = '*******'
         CALL RFILL (8, 1.0, DPLSCL)
         ALLPOL = .FALSE.
         ALLTIM = .FALSE.
         ALLIF = -1
         SMTYPE = ' '
         SMTIME(1) = 300.
         SMTIME(2) = 300.
         SMTIME(3) = 0.
         LOADIT = .TRUE.
         PNDING = .TRUE.
C                                       get attached class names
         PROBLM = OBJECT
         CALL EDIGET (OBJECT, 'UVMASTER', TYPE, DIM, DUMMY, UVMAST,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
C
         CALL EDIGET (OBJECT, 'UVSECOND', TYPE, DIM, DUMMY, UVDAT2,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         DO2ND = UVDAT2.NE.' '
         CALL EDIGET (OBJECT, 'TVDEVICE', TYPE, DIM, DUMMY, TVNAME,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDIGET (OBJECT, 'DOUVFLAG', TYPE, DIM, UVFLAG, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (.NOT.UVFLAG) THEN
            MSGTXT = 'EDITUV CALLED WITH UV FLAGGING TURNED OFF'
            IERR = 8
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Make sure that the data are
C                                       sorted correctly
         PROBLM = UVMAST
         CALL UVDGET (UVMAST, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (SORD(1:1).NE.'T') THEN
            MSGTXT = 'Sorting data to TB order'
            CALL MSGWRT (2)
            SORD = 'TB'
            CALL OUVPUT (UVMAST, 'SORT', OOACAR, DIM, DUMMY, SORD, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OUVSRT (UVMAST, UVMAST, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Get a source list
         CALL EDSLST (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Create FCFILE table object
         FCFILE = 'Flag Command table for UV edits'
         PROBLM = FCFILE
         CALL CREATE (FCFILE, 'TABLE', IERR)
         IF (IERR.NE.0) GO TO 980
         FCREAT = .TRUE.
C                                       copy adverbs to FCFILE
         CALL IN2OBJ (UVMAST, 4, INKT, INKT, FCFILE, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Extension type TBLTYPE
         INEXT = 'FC'
         DIM(1) = 2
         DIM(2) = 1
         CALL OPUT (FCFILE, 'TBLTYPE', OOACAR, DIM, DUMMY, INEXT, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       version always 1
         VERS = 1
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (FCFILE, 'VER', OOAINT, DIM, VERS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       create the FC table file
         CALL OBFEXS (FCFILE, PREXIS, IERR)
         IERR = 0
         IF (PREXIS) THEN
            MSGTXT = '**** WARNING: USING A PRE-EXISTING FC TABLE ****'
            CALL MSGWRT (8)
            END IF
         CALL OFCINI (FCFILE, 'WRIT', FLGNUM, FCROW, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL  OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT,
     *      LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.NE.0) GO TO 980
         FLGNMX = FLGNUM
C                                       Get IF channel selection
         PROBLM = UVDAT2
         IF (DO2ND) THEN
            CALL SECSLT (UVDAT2, BIF2, EIF2, BCHAN2, ECHAN2, STOKES,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       weight rescaling
         PROBLM = UVMAST
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OUVGET (UVMAST, 'REWEIGHT', TYPE, DIM, WTSCAL, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            WTSCAL = 1.0
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         IF (WTSCAL.LE.0.0) WTSCAL = 1.0
         MSGSUP = 32000
         CALL OUVGET (UVMAST, 'COMPARE', TYPE, DIM, DOCOMP, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            DOCOMP = .TRUE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         MSGSUP = 32000
         CALL OUVGET (UVMAST, 'DO3COLOR', TYPE, DIM, RDUM, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            RDUM = -1.0
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         MSGSUP = 32000
         CALL OUVGET (UVMAST, 'CROWDED', TYPE, DIM, CROWDP, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            CROWDP = .FALSE.
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 980
         CROWDI = CROWDP
         IF (.NOT.CROWDP) RDUM = -1.0
         DO3COL = RDUM.GT.0.0
         AL3COL = RDUM.GT.1.5
         IS3COL = DO3COL
         MSGSUP = 32000
         CALL OUVGET (UVMAST, 'REASON', TYPE, DIM, DUMMY, REASON, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            REASON = ' '
            IERR = 0
            END IF
         MSGSUP = 32000
         CALL OUVGET (UVMAST, 'DOEXPERT', TYPE, DIM, EXPERT, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            EXPERT = .FALSE.
            IERR = 0
            END IF
         IF ((EXPERT) .AND. (.NOT.CROWDP)) THEN
            ALLIF = 1
            ALLPOL = .TRUE.
            END IF
         IF (IERR.NE.0) GO TO 980
C                                       get other adverbs
         CALL OUVGET (UVMAST, 'FRQSEL', TYPE, DIM, FRQSEL, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (FRQSEL.EQ.0) FRQSEL = 1
         CALL SECSLT (UVMAST, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OUVGET (UVMAST, 'SUBARR', TYPE, DIM, SUBARR, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (SUBARR.LE.0) THEN
            SUBARR = 1
            CALL OUVPUT (UVMAST, 'CALEDIT.SUBARR', TYPE, DIM, SUBARR,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            MSGTXT = 'DOING SUBARRAY 1 ONLY'
            CALL MSGWRT (6)
            END IF
         CALL OUVGET (UVMAST, 'TIMRNG', TYPE, DIM, T, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         TSTART = T(1) + (T(2) + (T(3) + T(4)/60.) / 60.) / 24.
         TEND = T(5) + (T(6) + (T(7) + T(8)/60.) / 60.) / 24.
         IF (TEND.LE.TSTART) TEND = 999.
         CALL OUVGET (UVMAST, 'EQU_TIME', TYPE, DIM, DTIME, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (DTIME.LE.0.0) DTIME = 1.0 / 6000.0
         CALL OUVGET (UVMAST, 'GAP_TIME', TYPE, DIM, GTIME, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0 * DTIME)
         SMTIME(1) = GTIME * 60.0
         IFMAX = EIF - BIF + 1
         POLMAX = 1
         IF (STOKES.EQ.'HALF') POLMAX = 2
C                                       build descriptor string
         CALL OUVGET (UVMAST, 'NAME', TYPE, DIM, DUMMY, DDNAME(:12),
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL OUVGET (UVMAST, 'CLASS', TYPE, DIM, DUMMY, DDNAME(13:),
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL OUVGET (UVMAST, 'IMSEQ', TYPE, DIM, J, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL NAMEST (DDNAME, J, DDSTR, DDSLEN)
         DDTYPE = 'UV'
         DDSLEN = DDSLEN + 4
         DDSTR(DDSLEN:) = 'FQID'
         DDSLEN = DDSLEN + 5
         WRITE (MSGBUF,1030) FRQSEL
         CALL CHTRIM (MSGBUF, 8, MSGBUF, J)
         DDSTR(DDSLEN:) = MSGBUF(:J)
         DDSLEN = DDSLEN + J - 1
C                                       basic parameters
         TIMEM = 0
         TIME1 = 1
         TIME2 = 0
         ALLIN = .FALSE.
         ONEANT = .TRUE.
         PIFNOW = -1
         MAXREC = 0
         POLNOW = 1
         IF (CROWDP) POLNOW = 0
         LTYPE = 1
         LTYPE2 = 0
         IF (DOCOMP) LTYPE2 = 2
         IFNOW = BIF
         IF (CROWDI) IFNOW = 0
         PANTNW = 0
         CALL RFILL (8, 0.0, DPIXR)
         IF (EXPERT) THEN
            DPIXR(1,1) = 0.0
            DPIXR(2,1) = -1.0
            DPIXR(1,2) = -180.0 * DPLSCL(2)
            DPIXR(2,2) = 180.0 * DPLSCL(2)
            DPIXR(1,3) = 0.0
            DPIXR(2,3) = -1.0
            END IF
         ANTNOW = -1
C                                       Open terminal for conversation
         CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *      IERR)
         IF (IERR.NE.0) THEN
            TTY(2) = 0
            WRITE (MSGTXT,1035) IERR
            CALL MSGWRT (8)
            PROBLM = 'The terminal'
            GO TO 980
            END IF
         TTY(2) = MAX (1, TTY(2))
C                                       Graphics: menu, menu back,
C                                       editing, editdata, flagged data,
C                                       extra data, 2nd uv set
C                                       2 3 4 1 6 5 7
         GRSEL(1) = 6
         GRSEL(2) = 3
         GRSEL(3) = 4
         GRSEL(4) = 1
         GRSEL(5) = 5
         GRSEL(6) = 2
         GRSEL(7) = 0
         IF (DO2ND) GRSEL(7) = 7
C                                       learn about TV
         PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         MINWIN(1) = (36 + CEDG) * CSIZE(1) + 300
         MINWIN(2) = 41 * CSIZE(2) + 150
C                                       off all TV channels
         CALL TVDOPR (TVNAME, 'HOLD', I, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDOPR (TVNAME, 'INIT', I, IERR)
         IF (IERR.NE.0) GO TO 980
         I = 1
         CALL TVDOPR (TVNAME, 'TVOF', I, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       clear and on all graphics
         DO 45 I = 1,NGRPH-1
            CALL TVDOPR (TVNAME, 'GRON', I, IERR)
            IF (IERR.NE.0) GO TO 980
 45         CONTINUE
         CALL TVDFUN (TVNAME, 'OFFZ', 0, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDZOM (TVNAME, 'READ', NOZOOM, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (DO3COL) THEN
            CALL ED3CIN (IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Get some data and display it
         PROBLM = UVMAST
         ANTNOW = -1
         WASFLG = -1
         CALL EDGTUV (IERR)
         IF (IERR.NE.0) GO TO 980
         APOPEN = .TRUE.
         IF (POLMAX.LE.1) THEN
            CROWDP = .FALSE.
            POLNOW = 1
            END IF
         IF (.NOT.ALLIN) THEN
            CROWDI = .FALSE.
            IFNOW = BIF
            END IF
C                                       set the antennas
         MSGSUP = 32000
         CALL OUVGET (UVMAST, 'ANTS2USE', TYPE, DIM, AVAL, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            CALL FILL (50, 0, AVAL)
            IERR = 0
            END IF
         NUMPLT = 0
         IF ((AVAL(1).GT.0) .AND. (AVAL(1).LE.ANTMAX) .AND.
     *      (AVAL(2).GT.0) .AND. (AVAL(2).LE.ANTMAX)) THEN
            IF ((EDAFND (AVAL(1), ANTMLX, ANTEN)) .AND.
     *         (MSAMPS(AVAL(1)).GT.0) .AND. (MSAMPS(AVAL(2)).GT.0) .AND.
     *         (EDAFND(AVAL(2), ANTMLX, ANTEN))) THEN
               ANTNOW = AVAL(1)
               IF (WASXCR) THEN
                  NAPLT = NPLT + 1
               ELSE
                  NAPLT = NPLT
                  END IF
               DO 50 I = 2,NAPLT
                  IF ((AVAL(I).GT.0) .AND. (AVAL(I).LE.ANTMAX)) THEN
                     IF ((EDAFND (AVAL(I), ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(AVAL(I)).GT.0)) THEN
                        NUMPLT = NUMPLT + 1
                        PLTANS(NUMPLT) = AVAL(I)
                        IF (WASXCR) THEN
                           PLTAN(NUMPLT) = MAX (ANTNOW, PLTANS(NUMPLT))
     *                        + 256 * MIN (ANTNOW, PLTANS(NUMPLT))
                        ELSE
                           PLTAN(NUMPLT) = 257 * PLTANS(NUMPLT)
                           END IF
                        END IF
                     END IF
 50               CONTINUE
               END IF
            END IF
         IF (NUMPLT.LE.0) THEN
            IF (WASXCR) THEN
               NUMPLT = 3
               PLTANS(1) = ANTEN(2)
               PLTANS(2) = ANTEN(3)
               PLTANS(3) = ANTEN(4)
               PLTAN(1) = ANTNOW*256 + ANTEN(2)
               PLTAN(2) = ANTNOW*256 + ANTEN(3)
               PLTAN(3) = ANTNOW*256 + ANTEN(4)
            ELSE
               NUMPLT = 1
               PLTANS(1) = ANTEN(1)
               PLTAN(1) = 257 * ANTEN(1)
               IF (DOCOMP) LTYPE2 = 3
               END IF
            END IF
         ALLANT = ANTNOW
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
C                                       initial plot
         PROBLM = TVNAME
         CALL EDPLOT (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       set parameters for menu
         TVCOL = 2
         TVROWS(1) = NCOL1
         DO 60 I = 1,NCOL1
            CHOICS(I) = LIST1(I)
 60         CONTINUE
         J = TVROWS(1)
         IF (POLMAX*(EIF-BIF+1).GT.2) THEN
            J = J + 1
            CHOICS(J) = LIST2(1)
            END IF
         IF (POLMAX.GT.1) THEN
            J = J + 1
            CHOICS(J) = LIST2(2)
            J = J + 1
            CHOICS(J) = LIST2(3)
            END IF
         IF (EIF.GT.BIF) THEN
            J = J + 1
            CHOICS(J) = LIST2(4)
            IF (DIMIF.GT.1) THEN
               J = J + 1
               CHOICS(J) = LIST2(5)
               END IF
            END IF
         DO 70 I = 6,NCOL2
            J = J + 1
            CHOICS(J) = LIST2(I)
            IF (WASXCR) THEN
               IF (CHOICS(J).EQ.'SHOW PHASE') CHSHOW = J
               IF (CHOICS(J)(:10).EQ.'SHOW ALSO ') THEN
                  CHSHO2 = J
                  IF (.NOT.DOCOMP) J = J - 1
                  END IF
            ELSE
               IF (CHOICS(J).EQ.'NEXT BASELINE') J = J - 1
               IF (CHOICS(J).EQ.'SHOW PHASE') J = J - 1
               IF (CHOICS(J).EQ.'SHOW DIFF AMPL') CHSHOW = J
               IF (CHOICS(J)(:10).EQ.'SHOW ALSO ') J = J - 1
               END IF
 70         CONTINUE
         TVROWS(2) = J - TVROWS(1)
         IF (.NOT.DO2ND) TVROWS(2) = TVROWS(2) - 1
         ISHELP = 'EDIUV'
         TIMLIM = 0
         IG(1) = GRSEL(1)
         IG(2) = GRSEL(2)
         CALL FILL (NXPRT, 1, OPTOKX)
         OPTOKX(NXPRT-2) = -1
         IF (.NOT.WASXCR) OPTOKX(NXPRT-1) = -1
         IF ((POLMAX.EQ.1) .AND. (EIF.LE.BIF)) OPTOKX(NXPRT-5) = -1
C                                       LOOP POINT
C                                       window still big enough?
 100     PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
         I = LTVWND(4) - TOPLOT + 5 * CSIZE(2) - 1
         IF (NEWPLT) THEN
            CALL TVDOKA (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            GRSTAT(IG(1)) = 1
            CALL TVDRST (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            ILAST = I
            END IF
         IF (PREXIS) THEN
            CHOICE = 'REDO FLAGS'
         ELSE IF (EXPERT) THEN
            CALL TVDXPR (TVNAME, -1, TVCOL, TVROWS, IG, I, CHOICS,
     *         LEAVE, TTY, NXPRT, CHOICX, SHORTX, ALPHAX, OPTOKX,
     *         CHOICE, IERR)
            PROBLM = 'The terminal'
            IF (IERR.NE.0) GO TO 980
            J = TVROWS(1) + TVROWS(2)
            CALL EDCFND (CHOICE, J, CHOICS, CHS)
         ELSE
            TITLE = ' '
            NTITLE = 0
            SIDSEP = 7
            CALL TVDMEN (TVNAME, -1, TVCOL, TVROWS, IG, I, SIDSEP,
     *         ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHS,
     *         BUTTON, IERR)
            IF (IERR.NE.0) GO TO 980
            CHOICE = CHOICS(CHS)
            END IF
         CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
         IF (NEWPLT) ILAST = 0
C                                       Do something:
         IF (LOADIT) DOPLOT = .FALSE.
C                                       load/hold
         IF ((CHOICE.EQ.'HOLD TV LOAD') .OR.
     *      (CHOICE.EQ.'DO TV LOAD')) THEN
            IF (LOADIT) THEN
               IF (CHS.GT.0) CHOICS(CHS) = 'DO TV LOAD'
            ELSE
               IF (CHS.GT.0) CHOICS(CHS) = 'HOLD TV LOAD'
               END IF
            ILAST = 0
            LOADIT = .NOT.LOADIT
C                                       2nd UV on/off
         ELSE IF (CHOICE(:8).EQ.'2ND UV O') THEN
            IF (DO2ND) THEN
               IF (CHS.GT.0) CHOICS(CHS) = '2ND UV ON'
               CALL TVDOPR (TVNAME, 'GRCL', GRSEL(7), IERR)
            ELSE
               IF (CHS.GT.0) CHOICS(CHS) = '2ND UV OFF'
               DOPLOT = .TRUE.
               END IF
            ILAST = 0
            DO2ND = .NOT.DO2ND
C                                       interaction
         ELSE IF (CHOICE(:5).EQ.'FLAG ') THEN
            IF (PNDING) THEN
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (CHOICE.EQ.'FLAG TIME') THEN
               CALL EDFLTI (IERR)
            ELSE IF (CHOICE.EQ.'FLAG TIME RANGE') THEN
               CALL EDFLTR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG BELOW') THEN
               CALL EDFLFB (IERR)
            ELSE IF (CHOICE.EQ.'FLAG ABOVE') THEN
               CALL EDFLFA (IERR)
            ELSE IF (CHOICE.EQ.'FLAG AREA') THEN
               CALL EDFLAR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG POINT') THEN
               CALL EDFLPT (IERR)
            ELSE IF (CHOICE.EQ.'FLAG QUICKLY') THEN
               CALL EDFLQU (IERR)
            ELSE
               CALL EDSTUB ('FLAG', CHOICE, OBJECT)
               END IF
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       error bar plot
         ELSE IF (CHOICE.EQ.'PLOT ERROR BARS') THEN
            DOERRB = .NOT.DOERRB
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       list FC table
         ELSE IF (CHOICE.EQ.'LIST FLAGS') THEN
            CALL EDFCLI (IERR)
C                                       Undo flags in FC table
         ELSE IF (CHOICE.EQ.'UNDO FLAGS') THEN
            IF (FLGNMX.LE.1) THEN
               IERR = 0
               IFLN(1) = FLGNMX
               IFLN(2) = FLGNMX
            ELSE
               WRITE (MSGBUF,1100) FLGNMX
               CALL INQINT (TTY, MSGBUF, 2, IFLN, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
               IF (IFLN(2).LT.IFLN(1)) IFLN(2) = IFLN(1)
               END IF
            IF ((IERR.EQ.0) .AND. (IFLN(1).GT.0)) THEN
               PROBLM = TVNAME
               CALL EDFCUN (IFLN, IERR)
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Redo flags in FC table
         ELSE IF (CHOICE.EQ.'REDO FLAGS') THEN
            CALL EDFCDO (IERR)
            PREXIS = .FALSE.
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'ENTER ANTENNA') THEN
            MSGBUF = 'Enter number of main antenna to be edited'
            CALL INQINT (TTY, MSGBUF, 1, I, IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.LT.0) THEN
               I = ANTNOW
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
            IF ((I.NE.ANTNOW) .AND. (EDAFND (I, ANTMLX, ANTEN)) .AND.
     *         (MSAMPS(I).GT.0)) THEN
               PANTNW = ANTNOW
               ANTNOW = I
               LT1 = 0
               DO 105 J = 1,NUMPLT
 104              IF ((.NOT.WASACR) .AND. (ANTNOW.EQ.PLTANS(J))) THEN
                     CALL COPY (NUMPLT-J, PLTANS(J+1), PLTANS(J))
                     PLTANS(NUMPLT) = 0
                     GO TO 104
                     END IF
                  IF (PLTANS(J).GT.0) LT2 = J
                  LT1 = MAX (LT1, PLTANS(J))
 105              CONTINUE
 106           IF (LT2.LT.NUMPLT) THEN
                  I = LT1
                  DO 108 K = 1,ANTMAX
                     I = I + 1
                     IF (I.GT.ANTMAX) I = 1
C                                       good antenna - used already?
                     IF ((EDAFND (I, ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(I).GT.0)) THEN
                        DO 107 J = 1,LT2
                           IF (I.EQ.PLTANS(J)) GO TO 108
 107                       CONTINUE
                        IF ((I.EQ.ANTNOW) .AND. (.NOT.WASACR)) GO TO 108
C                                       found a new one
                        LT2 = LT2 + 1
                        PLTANS(LT2) = I
                        LT1 = MAX (LT1, I)
                        GO TO 106
                        END IF
 108                 CONTINUE
                  NUMPLT = LT2
                  END IF
               IF (WASXCR) THEN
                  DO 110 J = 1,NUMPLT
                     PLTAN(J) = MIN (ANTNOW, PLTANS(J)) * 256
     *                  + MAX (ANTNOW, PLTANS(J))
 110                 CONTINUE
               ELSE
                  PLTAN(1) = 257 * ANTNOW
                  END IF
               IF (ALLANT.LT.0) ALLANT = -PLTAN(1)
               IF (ALLANT.GT.0) ALLANT = ANTNOW
               IF (ONEANT) THEN
                  PROBLM = UVMAST
                  CALL EDGTUV (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'NEXT ANTENNA') THEN
            I = ANTNOW
            DO 101 J = 1,ANTMAX
               I = I + 1
               IF (I.GT.ANTMAX) I = 1
               IF ((EDAFND (I, ANTMLX, ANTEN)) .AND. (MSAMPS(I).GT.0))
     *            GO TO 102
 101           CONTINUE
            I = ANTNOW
 102        IF ((I.NE.ANTNOW) .AND. (EDAFND (I, ANTMLX, ANTEN)) .AND.
     *         (MSAMPS(I).GT.0)) THEN
               PANTNW = ANTNOW
               ANTNOW = I
               LT1 = 0
               DO 115 J = 1,NUMPLT
 114              IF ((.NOT.WASACR) .AND. (ANTNOW.EQ.PLTANS(J))) THEN
                     CALL COPY (NUMPLT-J, PLTANS(J+1), PLTANS(J))
                     PLTANS(NUMPLT) = 0
                     GO TO 114
                     END IF
                  IF (PLTANS(J).GT.0) LT2 = J
                  LT1 = MAX (LT1, PLTANS(J))
 115              CONTINUE
 116           IF (LT2.LT.NUMPLT) THEN
                  I = LT1
                  DO 118 K = 1,ANTMAX
                     I = I + 1
                     IF (I.GT.ANTMAX) I = 1
C                                       good antenna - used already?
                     IF ((EDAFND (I, ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(I).GT.0)) THEN
                        DO 117 J = 1,LT2
                           IF (I.EQ.PLTANS(J)) GO TO 118
 117                       CONTINUE
                        IF ((I.EQ.ANTNOW) .AND. (.NOT.WASACR)) GO TO 118
C                                       found a new one
                        LT2 = LT2 + 1
                        PLTANS(LT2) = I
                        LT1 = MAX (LT1, I)
                        GO TO 116
                        END IF
 118                 CONTINUE
                  NUMPLT = LT2
                  END IF
               IF (WASXCR) THEN
                  DO 120 J = 1,NUMPLT
                     PLTAN(J) = MIN (ANTNOW, PLTANS(J)) * 256
     *                  + MAX (ANTNOW, PLTANS(J))
 120                 CONTINUE
               ELSE
                  PLTAN(1) = 257 * ANTNOW
                  END IF
               IF (ALLANT.LT.0) ALLANT = -PLTAN(1)
               IF (ALLANT.GT.0) ALLANT = ANTNOW
               IF (ONEANT) THEN
                  PROBLM = UVMAST
                  CALL EDGTUV (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       next baseline in set
         ELSE IF (CHOICE.EQ.'NEXT BASELINE') THEN
            IF (WASXCR) THEN
               I = PLTANS(1)
               CALL COPY (NUMPLT-1, PLTANS(2), PLTANS(1))
               LT2 = NUMPLT - 1
               PLTANS(NUMPLT) = 0
               IF ((.NOT.WASACR) .AND. (ANTNOW.EQ.PLTANS(1))) THEN
                  CALL COPY (NUMPLT-1, PLTANS(2), PLTANS(1))
                  PLTANS(NUMPLT) = 0
                  LT2 = LT2 - 1
                  END IF
               LT1 = 0
               DO 121 J = 1,LT2
                  LT1 = MAX (LT1, PLTANS(J))
 121              CONTINUE
C                                       fill in list
 126           IF (LT2.LT.NUMPLT) THEN
                  I = LT1
                  DO 128 K = 1,ANTMAX
                     I = I + 1
                     IF (I.GT.ANTMAX) I = 1
C                                       good antenna - used already?
                     IF ((EDAFND (I, ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(I).GT.0)) THEN
                        DO 127 J = 1,LT2
                           IF (I.EQ.PLTANS(J)) GO TO 128
 127                       CONTINUE
                        IF ((I.EQ.ANTNOW) .AND. (.NOT.WASACR)) GO TO 128
C                                       found a new one
                        LT2 = LT2 + 1
                        PLTANS(LT2) = I
                        LT1 = MAX (LT1, I)
                        GO TO 126
                        END IF
 128                 CONTINUE
                  NUMPLT = LT2
                  END IF
               DO 130 J = 1,NUMPLT
                  PLTAN(J) = MIN (ANTNOW, PLTANS(J)) * 256
     *               + MAX (ANTNOW, PLTANS(J))
 130              CONTINUE
               IF (ALLANT.LT.0) ALLANT = -PLTAN(1)
               IF (ALLANT.GT.0) ALLANT = ANTNOW

               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Other ants
         ELSE IF (CHOICE.EQ.'ENTER OTHER ANT') THEN
            IF (WASXCR) THEN
               NAPLT = NPLT
               WRITE (MSGBUF,1220) NAPLT
            ELSE
               NAPLT = NPLT - 1
               WRITE (MSGBUF,1221) NAPLT
               END IF
            CALL INQINT (TTY, MSGBUF, NAPLT, IVAL, IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.LT.0) THEN
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
            ELSE
               K = NUMPLT
               IF (WASXCR) THEN
                  NUMPLT = 0
               ELSE
                  NUMPLT = 1
                  END IF
               DO 135 I = 1,NAPLT
                  IF ((IVAL(I).GT.0) .AND. (IVAL(I).LE.ANTMAX)) THEN
                     IF ((EDAFND (IVAL(I), ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(IVAL(I)).GT.0)) THEN
                        NUMPLT = NUMPLT + 1
                        PLTANS(NUMPLT) = IVAL(I)
                        IF (WASXCR) THEN
                           PLTAN(NUMPLT) = MAX (ANTNOW, PLTANS(NUMPLT))
     *                        + 256 * MIN (ANTNOW, PLTANS(NUMPLT))
                        ELSE
                           PLTAN(NUMPLT) = 257 * PLTANS(NUMPLT)
                           END IF
                        END IF
                     END IF
 135              CONTINUE
               IF (ALLANT.LT.0) ALLANT = -PLTAN(1)
               IF (ALLANT.GT.0) ALLANT = ANTNOW
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               IF (K.NE.NUMPLT) THEN
                  CALL EDWINC (MINWIN(1), MINWIN(2), IERR)
                  IF (IERR.NE.0) GO TO 980
                  NEWPLT = .TRUE.
                  ILAST = 0
                  END IF
               END IF
C                                       Select IF
         ELSE IF (CHOICE.EQ.'ENTER IF') THEN
            IF ((EIF.EQ.BIF+1) .AND. (.NOT.CROWDI)) THEN
               I = 2 * BIF + 1 - IFNOW
               IERR = 0
            ELSE IF ((EIF.EQ.BIF+1) .AND. (CROWDI)) THEN
               IF (IFNOW.EQ.0) THEN
                  I = BIF
               ELSE IF (IFNOW.EQ.EIF) THEN
                  I = 0
               ELSE
                  I = EIF
                  END IF
               IERR = 0
            ELSE
               MSGBUF = 'Enter IF number to be plotted and edited'
               CALL INQINT (TTY, MSGBUF, 1, I, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
               IF (IERR.LT.0) THEN
                  I = IFNOW
                  MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                  CALL MSGWRT (6)
                  IERR = 0
                  END IF
               END IF
            IF (((I.LT.BIF) .OR. (I.GT.EIF)) .AND. ((I.NE.0) .OR.
     *         (.NOT.CROWDI))) THEN
               WRITE (MSGTXT,1210) 'IF', I, BIF, EIF
               CALL MSGWRT (6)
            ELSE IF (I.NE.IFNOW) THEN
               PNDING = .TRUE.
               PIFNOW = IFNOW
               IFNOW = I
               IF ((ALLIF.EQ.0) .AND. ((IFNOW.LT.FGIFRS(1)) .OR.
     *            (IFNOW.GT.FGIFRS(2)))) THEN
                  WRITE (MSGTXT,1135) IFNOW, FGIFRS
                  CALL MSGWRT (7)
                  END IF
               IF (.NOT.ALLIN) THEN
                  PROBLM = UVMAST
                  CALL EDGTUV (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               DOPLOT = .TRUE.
               END IF
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Smooth parameters
         ELSE IF (CHOICE.EQ.'SET SCAN LENGTH') THEN
            MSGBUF = 'Enter ''scan'' length in seconds'
            CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
            IF (IERR.GT.0) GO TO 980
C                                       recompute the differences
            IF ((IERR.EQ.0) .AND. (DVAL(1).GT.0) .AND.
     *         (DVAL(1).LT.86400) .AND. (DVAL(1).NE.SMTIME(1))) THEN
               SMTIME(1) = DVAL(1)
               FLAGED = .TRUE.
               CALL EDPLST (GRSEL(6), IERR)
               DOPLOT = (LTYPE.EQ.3) .OR. (DOCOMP .AND. (LTYPE2.EQ.3))
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Set reason
         ELSE IF (CHOICE.EQ.'SET REASON') THEN
            MSGBUF = 'Enter new reason, left justified'
 136        CALL INQSTR (TTY, MSGBUF, 24, REAZON, IERR)
            IF (IERR.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 136
               END IF
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF ((IERR.EQ.0) .AND. (REAZON.NE.'-')) THEN
               REASON = REAZON
               I = ITRIM (REASON)
               I = MAX (1, I)
               MSGTXT = 'Using reason = ''' // REASON(:I) // ''''
               CALL MSGWRT (2)
            ELSE
               MSGTXT = 'ERROR IN ANSWER: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Set ranges
         ELSE IF (CHOICE(:6).EQ.'ENTER ') THEN
            I = 1
            IF (CHOICE(7:).EQ.'PHASE RNG') I = 2
            IF (CHOICE(7:).EQ.'DAMP RNG') I = 3
            J = ITRIM (DTYPE(I))
            MSGBUF = 'Enter ' // DTYPE(I)(:J) // ' display range in ' //
     *         DUNITS(I)
            CALL INQFLT (TTY, MSGBUF, 2, DVAL, IERR)
            PROBLM = 'The terminal'
            IF (IERR.GT.0) GO TO 980
            IF (IERR.EQ.0) THEN
               DPIXR(1,I) = DVAL(1) * DPLSCL(I)
               DPIXR(2,I) = DVAL(2) * DPLSCL(I)
               IF ((I.EQ.LTYPE) .OR. (DOCOMP .AND. (I.EQ.LTYPE2))) THEN
                  DOPLOT = .TRUE.
                  PNDING = .TRUE.
                  END IF
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Switch polarization
         ELSE IF (CHOICE.EQ.'SWITCH POLARIZ') THEN
            IF (CROWDP) THEN
               POLNOW = MOD (POLNOW+1, 3)
            ELSE
               POLNOW = 3 - POLNOW
               END IF
            DOPLOT = .TRUE.
            PNDING = .TRUE.
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Next Correlator
         ELSE IF (CHOICE.EQ.'NEXT POL/IF') THEN
            PNDING = .TRUE.
            IF (POLNOW.LT.POLMAX) THEN
               POLNOW = POLNOW + 1
               IFNOW = MAX (BIF, IFNOW)
            ELSE
               POLNOW = 1
               IF (CROWDP) POLNOW = 0
               PIFNOW = IFNOW
               IFNOW = MAX (BIF, IFNOW+1)
               IF (IFNOW.GT.EIF) THEN
                  IFNOW = BIF
                  IF (CROWDI) IFNOW = 0
                  END IF
               PROBLM = UVMAST
               IF (.NOT.ALLIN) THEN
                  CALL EDGTUV (IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               END IF
            DOPLOT = .TRUE.
            IS3COL = ((IFNOW.EQ.0) .OR. (POLNOW.EQ.0)) .AND. DO3COL
C                                       Switch all ant
         ELSE IF (CHOICE.EQ.'ROTATE ALL ANT') THEN
            IF (ALLANT.GT.0) THEN
               ALLANT = -PLTAN(1)
            ELSE IF (ALLANT.EQ.0) THEN
               ALLANT = ANTNOW
            ELSE
               ALLANT = 0
               END IF
            CALL EDPLST (GRSEL(6), IERR)
C                                       Switch all pol
         ELSE IF (CHOICE.EQ.'SWITCH ALL POL') THEN
            IF (POLMAX.GT.1) THEN
               ALLPOL = .NOT.ALLPOL
               CALL EDPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all IF
         ELSE IF (CHOICE.EQ.'SWITCH ALL IF') THEN
            IF (DIMIF.GT.1) THEN
               IF ((DIMIF.GT.2) .AND. (ALLIN)) THEN
                  ALLIF = ALLIF + 1
                  IF (ALLIF.EQ.2) THEN
                     ALLIF = -1
                  ELSE IF (ALLIF.EQ.0) THEN
                     MSGBUF = 'Enter IF number range to flag'
                     CALL INQINT (TTY, MSGBUF, 2, ITEMP, IERR)
                     PROBLM = 'The terminal'
                     IF (IERR.GT.0) GO TO 980
                     IF (IERR.EQ.0) THEN
                        IF ((ITEMP(1).EQ.0) .AND. (ITEMP(2).EQ.0)) THEN
                           ALLIF = -1
                        ELSE IF ((ITEMP(2).LT.ITEMP(1)) .OR.
     *                     (ITEMP(1).LT.BIF) .OR. (ITEMP(1).GT.EIF) .OR.
     *                     (ITEMP(2).LT.BIF) .OR. (ITEMP(2).GT.EIF))
     *                     THEN
                           IERR = -1
                        ELSE
                           FGIFRS(1) = ITEMP(1)
                           FGIFRS(2) = ITEMP(2)
                           IF ((IFNOW.LT.FGIFRS(1)) .OR.
     *                        (IFNOW.GT.FGIFRS(2))) THEN
                              WRITE (MSGTXT,1135) IFNOW, FGIFRS
                              CALL MSGWRT (7)
                              END IF
                           END IF
                        END IF
                     IF (IERR.LT.0) THEN
                        I = IFNOW
                        MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
                        CALL MSGWRT (6)
                        IERR = 0
                        END IF
                     END IF
               ELSE
                  ALLIF = -ALLIF
                  END IF
               CALL EDPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all time
         ELSE IF (CHOICE.EQ.'SWITCH ALL TIME') THEN
            ALLTIM = .NOT.ALLTIM
            CALL EDPLST (GRSEL(6), IERR)
C                                       Switch all source
         ELSE IF (CHOICE.EQ.'SWITCH ALL SOURC') THEN
            ALLSOR = .NOT.ALLSOR
            CALL EDPLST (GRSEL(6), IERR)
C                                       TV zoom
         ELSE IF (CHOICE.EQ.'TV ZOOM') THEN
            CALL OTVZOM (TVNAME, IERR)
            CALL TVDZOM (TVNAME, 'READ', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       Off zoom
         ELSE IF (CHOICE.EQ.'OFF ZOOM') THEN
            CALL OTVOFZ (TVNAME, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'READ', SVZOOM, IERR)
C                                       expert mode switch
         ELSE IF (CHOICE.EQ.'USE EXPERT MODE') THEN
            EXPERT = .NOT.EXPERT
C                                       full plot
         ELSE IF (CHOICE.EQ.'PLOT ALL TIMES') THEN
            IF ((TIME1.GT.1) .OR. (TIME2.LT.TIMEM)) THEN
               TIME1 = 1
               TIME2 = TIMEM-1
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Select frame
         ELSE IF (CHOICE.EQ.'SELECT FRAME') THEN
            IF ((TIME1.GT.1) .OR. (TIME2.LT.TIMEM-1) .OR. PNDING) THEN
               TIME1 = 1
               TIME2 = TIMEM-1
               CALL EDPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL EDFRAM (IERR)
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Next frame
         ELSE IF (CHOICE.EQ.'NEXT FRAME') THEN
            LT2 = MIN (2*TIME2-TIME1, TIMEM-1)
            LT1 = LT2 - TIME2 + TIME1
            IF ((TIME1.NE.LT1) .OR. (TIME2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               TIME1 = LT1
               TIME2 = LT2
               END IF
C                                       Previous frame
         ELSE IF (CHOICE.EQ.'PREVIOUS FRAME') THEN
            LT1 = MAX (2*TIME1-TIME2, 1)
            LT2 = LT1 + TIME2 - TIME1
            IF ((TIME1.NE.LT1) .OR. (TIME2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               TIME1 = LT1
               TIME2 = LT2
               END IF
C                                       Show a type
         ELSE IF (CHOICE(:10).EQ.'SHOW ALSO ') THEN
            IF (DOCOMP) THEN
               I = LTYPE2
               IF (CHOICE(11:).EQ.'AMPL') LTYPE2 = 1
               IF ((CHOICE(11:).EQ.'PHASE') .AND. (WASXCR)) LTYPE2 = 2
               IF (CHOICE(11:).EQ.'DAMP') LTYPE2 = 3
               IF ((I.NE.LTYPE2) .OR. ((LTYPE2.GT.2) .AND. (FLAGED)))
     *            THEN
                  IF (LTYPE.EQ.2) THEN
                     I = 1
                     IF (LTYPE2.EQ.1) I = 3
                  ELSE
                     I = 6 - LTYPE - LTYPE2
                     END IF
                  CHOICS(CHSHO2) = SHOW2(I)
                  DOPLOT = .TRUE.
                  PNDING = .TRUE.
                  END IF
               END IF
C                                       Show a type
         ELSE IF (CHOICE(:5).EQ.'SHOW ') THEN
            I = LTYPE
            IF (CHOICE(6:).EQ.'AMPLITUDE') LTYPE = 1
            IF ((CHOICE(6:).EQ.'PHASE') .AND. (WASXCR)) LTYPE = 2
            IF (CHOICE(6:).EQ.'DIFF AMPL') LTYPE = 3
            IF ((I.NE.LTYPE) .OR. ((LTYPE.GT.2) .AND. (FLAGED))) THEN
               OPTOKX(12+I) = 1
               OPTOKX(12+LTYPE) = -1
               IF (WASXCR) THEN
                  CHOICS(CHSHOW) = SHOW(1)
                  CHOICS(CHSHOW+1) = SHOW(3)
                  IF (LTYPE.EQ.1) THEN
                     CHOICS(CHSHOW) = SHOW(2)
                  ELSE IF (LTYPE.EQ.3) THEN
                     CHOICS(CHSHOW+1) = SHOW(2)
                     END IF
                  IF (DOCOMP) THEN
                     IF (LTYPE.EQ.LTYPE2) THEN
                        IF (LTYPE.EQ.2) THEN
                           LTYPE2 = 1
                        ELSE
                           LTYPE2 = 2
                           END IF
                        END IF
                     I = 6 - LTYPE - LTYPE2
                     CHOICS(CHSHO2) = SHOW2(I)
                     END IF
               ELSE
                  LTYPE2 = 4 - LTYPE
                  CHOICS(CHSHOW) = SHOW(LTYPE2)
                  END IF
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Replot
         ELSE IF (CHOICE.EQ.'REPLOT') THEN
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Exit
         ELSE IF (CHOICE.EQ.'EXIT') THEN
            IERR = 0
            GO TO 995
C                                       Abort
         ELSE IF (CHOICE.EQ.'ABORT') THEN
            IERR = -1
            GO TO 995
            END IF
         IF ((IERR.LE.0) .AND. (DOPLOT)) THEN
            IF (LOADIT) THEN
               CALL EDPLOT (IERR)
            ELSE IF (PNDING) THEN
               CALL EDPLST (GRSEL(6), IERR)
               END IF
            PROBLM = TVNAME
            END IF
         IF (IERR.GT.0) GO TO 980
         GO TO 100
C                                       apply result
      ELSE IF (OPCODE.EQ.'APPL') THEN
         PROBLM = UVMAST
         CALL EDFCAP (UVMAST, IERR)
         IF (IERR.NE.0) GO TO 980
         PROBLM = FCFILE
         CALL TABZAP (FCFILE, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'EDGTUV', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 800 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 800        CONTINUE
         IF (DO3COL) THEN
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+1, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+2, IERR)
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       do not apply, don't destroy
      ELSE IF ((OPCODE.EQ.'ABOR') .OR. (OPCODE.EQ.'KILL')) THEN
         IF ((FCREAT) .AND. (OPCODE.NE.'KILL')) THEN
            CALL TABZAP (FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'EDGTUV', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 810 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 810        CONTINUE
         IF (DO3COL) THEN
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+1, IERR)
            CALL TVDOPR (TVNAME, 'TVOFF', CPLANE+2, IERR)
            END IF
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       ??
      ELSE
         IERR = 2
         CALL EDSTUB ('EDITUV', OPCODE, OBJECT)
         END IF
      GO TO 995
C                                       error
 980  MSGTXT = 'EDITUV: ERROR WITH OBJECT ' // PROBLM
      CALL MSGWRT (7)
C
 995  IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), J)
      IF (GRSEL(8).GT.0) CALL TVDGRC (TVNAME, 'WRIT', GRSEL(8), RGB8,
     *   JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (I8)
 1035 FORMAT ('ERROR',I6,' OPENING THE TERMINAL')
 1100 FORMAT ('Enter flag command number range to be undone: 1 to',I5)
 1135 FORMAT ('WARNING: CURRENT IF',I3,' OUTSIDE FLAG RANGE',2I3)
 1210 FORMAT (A,I5,' OUT OF RANGE',I4,' to',I4)
 1220 FORMAT ('Enter ',I2,' 2nd antenna numbers to display')
 1221 FORMAT ('Enter ',I2,' other antenna autocorrelations to display')
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   Private functions:
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ED3CIN (IERR)
C-----------------------------------------------------------------------
C   ED3CIN initializes things for 3-color
C   Outputs:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I, N, I1, I2
      REAL      X, DX
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       turn on 3-color channels
      CPLANE = NGRAY - 2
      CALL TVDOPR (TVNAME, 'TV3C', CPLANE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       when 2 colors
      N = 2
      DX = 1.0 / 2.5
      X = DX / 2.0
      CALL COLOR3 (X, .FALSE., COLOR2(1,1))
      X = X + DX
      CALL COLOR3 (X, .FALSE., COLOR2(1,2))
C                                       when all colors
      N = IFMAX * POLMAX
      DX = 1.0 / MAX (1.0, N+0.5)
      X = DX / 2.0
      I1 = (BIF - 1) * POLMAX + 1
      I2 = EIF * POLMAX
      DO 20 I = I1,I2
         CALL COLOR3 (X, .FALSE., COLORS(1,I))
         X = X + DX
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ED3LIN (TYPE, CHAN, NPOINT, X, Y, IERR)
C-----------------------------------------------------------------------
C   ED3LIN decides whether to plot in 3 colors or graphics and calls
C   the appropriate TVDEVICE function to draw lines
C   Call only for edit data
C   Inputs
C      TYPE     I      type of draw: 1,2 turn on line, 3,4 turn off
C      CHAN     I      Graphics channel to use (if graphics)
C      NPOINT   I      Number of points
C      X        I(*)   Vertex X values
C      Y        I(*)   Vertex Y values
C   Outputs
C      IERR     I      Error code
C-----------------------------------------------------------------------
      INTEGER   TYPE, CHAN, NPOINT, X(*), Y(*), IERR
C
      INCLUDE 'EDIUTIL.INC'
      INTEGER   I
C-----------------------------------------------------------------------
C                                       color
C      IF ((DO3COL) .AND. ((POLNOW.EQ.0) .OR. (IFNOW.EQ.0))) THEN
      IF ((IS3COL) .OR. (AL3COL)) THEN
         IF ((POLNOW.EQ.0) .AND. (IFNOW.NE.0)) THEN
            I = MOD (CCOLOR-1,2) + 1
            CALL TVDLN3 (TVNAME, TYPE, CPLANE, COLOR2(1,I), NPOINT,
     *         X, Y, IERR)
         ELSE
            CALL TVDLN3 (TVNAME, TYPE, CPLANE, COLORS(1,CCOLOR), NPOINT,
     *         X, Y, IERR)
            END IF
C                                       graphis
      ELSE
         CALL TVDLIN (TVNAME, TYPE, CHAN, NPOINT, X, Y, IERR)
         END IF
C
 999  RETURN
      END
      LOGICAL FUNCTION EDAFND (IA, NANT, ANTENS)
C-----------------------------------------------------------------------
C   returns true if antenna IA in list ANTENS(NANT)
C   Inputs:
C      IA       I      test number
C      NANT     I      length of list
C      ANTENS   I(*)   list of numbers
C   Output:
C      EDAFND   L      number is in list
C-----------------------------------------------------------------------
      INTEGER   IA, NANT, ANTENS(*)
C
      INTEGER   I
      INTEGER    DEBUG(90)
C-----------------------------------------------------------------------
      EDAFND = .FALSE.
      IF (NANT.GT.0) THEN
         CALL COPY (NANT, ANTENS, DEBUG)
         DO 10 I = 1,NANT
            IF (IA.EQ.ANTENS(I)) EDAFND = .TRUE.
 10         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDANTS (ANTS, ANT1, ANT2)
C-----------------------------------------------------------------------
C   Breaks 256*m + n into its parts
C   Input:
C      ANTS   I   Input baseline number: 256 * M + N
C   Output
C      ANT1   I   Lower antenna number (> 0)
C      ANT2   I   Higher antenna number (or 0)
C-----------------------------------------------------------------------
      INTEGER   ANTS, ANT1, ANT2
C
      INTEGER   N1, N2
C-----------------------------------------------------------------------
      N1 = ANTS / 256
      N2 = MOD (ANTS, 256)
      IF ((N1.EQ.0) .OR. (N2.EQ.0)) THEN
         ANT1 = MAX (N1, N2)
         ANT2 = 0
      ELSE
         ANT1 = MIN (N1, N2)
         ANT2 = MAX (N1, N2)
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDCFND (CHOICE, N, CHOICS, CHS)
C-----------------------------------------------------------------------
C   find a string in a list of strings
C   Input:
C      CHOICE   C*(*)      String to find
C      N        I          Number of strings in list
C      CHOICS   C(N)*(*)   List of strings
C   Output:
C      CHS      I          Index into list or zero if not found
C-----------------------------------------------------------------------
      INTEGER   N, CHS
      CHARACTER CHOICE*(*), CHOICS(N)*(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      CHS = 0
      DO 10 I = 1,N
         IF (CHOICE.EQ.CHOICS(I)) THEN
            CHS = I
            GO TO 999
            END IF
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE EDDBOX (NAME, TYPE, MODE, CHAN, CORN, IERR)
C-----------------------------------------------------------------------
C   draw a box on the TV
C      NAME     C*?    Open TV device object
C      TYPE     I      Type of op: 1-4 boxes, 5 plus
C      MODE     I      Mode of drawing:
C                         1 draw on graphics, 2 draw on grey channel
C                         3 erase on graphics, 4 erase on gray
C      CHAN     I      channel number: default GRCHNS(1), TVCHNS(1)
C      CORN     I(4)   BLC: x,y then TRC: x,y
C   Outputs
C      IERR     I      error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   TYPE, MODE, CHAN, CORN(4), IERR
C
      INTEGER   IX(5), IY(5)
C-----------------------------------------------------------------------
      IF ((TYPE.NE.5) .AND. (TYPE.NE.6)) THEN
         IX(1) = CORN(1)
         IX(2) = CORN(3)
         IX(3) = CORN(3)
         IX(4) = CORN(1)
         IX(5) = CORN(1)
         IY(1) = CORN(2)
         IY(2) = CORN(2)
         IY(3) = CORN(4)
         IY(4) = CORN(4)
         IY(5) = CORN(2)
         CALL TVDLIN (NAME, MODE, CHAN, 5, IX, IY, IERR)
      ELSE
         IX(1) = CORN(1) - CORN(3)
         IX(2) = CORN(1) + CORN(3)
         IY(1) = CORN(2)
         IY(2) = CORN(2)
         CALL TVDLIN (NAME, MODE, CHAN, 2, IX, IY, IERR)
         IF (IERR.NE.0) GO TO 999
         IX(1) = CORN(1)
         IX(2) = CORN(1)
         IY(1) = CORN(2) - CORN(4)
         IY(2) = CORN(2) + CORN(4)
         CALL TVDLIN (NAME, MODE, CHAN, 2, IX, IY, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDFCAP (UVDATA, IERR)
C-----------------------------------------------------------------------
C   Applies an FC table to a uv data set
C   Inputs:
C      UVDATA   C*(*)   UV master data set to be flagged
C   Output:
C      IERR     I       Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C   UV Editor common is used: FCFILE, UVFLAG, ...
C   The FCFILE is left to later disposal.
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      INTEGER   IERR
C
      INTEGER   NKEY1
      PARAMETER (NKEY1 = 4)
      CHARACTER FGFILE*32, CDUMMY*1, OUTK1(NKEY1)*8, FGVER(3)*32,
     *   LINE*72, PROBLM*32, INEXT*2, REAZON*24, FIFILE*32
      INTEGER   DIM(7), TYPE, VERS, FCOPEN, FGOPEN, JERR, LASTR, IROW,
     *   FGROW, IT(6), FGANTS(2), I, DUMMY, NADD, MSGSAV, COLS(2),
     *   ITT(8), FGVERI, FIOPEN, FIROW, NROWI
      REAL      DOHIST
      LOGICAL   PFLAGS(4)
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIFCPS.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Adverbs to copy from UVDATA
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
      DATA FGVER /'IN_FGVER', 'OUT_FGVER', 'COP_FGVER'/
C-----------------------------------------------------------------------
      IF (.NOT.UVFLAG) GO TO 999
      FCOPEN = 0
      FGOPEN = 0
      FIOPEN = 0
C                                       set up reason
      CALL ZDATE (IT(4))
      CALL ZTIME (IT(1))
      IT(4) = -IT(4)
      REAZON = TSKNAM
      CALL TIMDAT (IT(1), IT(4), REAZON(17:24), REAZON(7:15))
C                                       open FC FILE
      PROBLM = FCFILE
      CALL OFCINI (FCFILE, 'READ', FLGNUM, LASTR, IERR)
      IF (IERR.NE.0) GO TO 980
      FCOPEN = 1
      IF ((LASTR.LE.0) .OR. (FLGNUM.LE.0)) GO TO 995
C                                       Do we do history?
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OGET (UVDATA, 'DOHIST', TYPE, DIM, DOHIST, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         DOHIST = -1.0
         IERR = 0
         END IF
C                                       Create FGFILE object
      FGFILE = 'FG table to be used to edit'
      PROBLM = FGFILE
      CALL CREATE (FGFILE, 'TABLE', IERR)
      IF (IERR.NE.0) GO TO 980
      FGOPEN = 1
C                                       copy basic adverbs
      PROBLM = UVDATA
      CALL IN2OBJ (UVDATA, NKEY1, OUTK1, OUTK1, FGFILE, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       set version, type
      CALL OGET (UVDATA, FGVER(3), TYPE, DIM, FGVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL OGET (UVDATA, FGVER(2), TYPE, DIM, VERS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (VERS.LT.0) VERS = 1
      PROBLM = FGFILE
      CALL OPUT (FGFILE, 'VER', TYPE, DIM, VERS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      INEXT = 'FG'
      DIM(1) = 2
      CALL OPUT (FGFILE, 'TBLTYPE', OOACAR, DIM, DUMMY, INEXT, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       History
      IF (DOHIST.GT.-9.5) THEN
         CALL OHTIME (UVDATA, JERR)
         IF (JERR.NE.0) DOHIST = -10.0
         END IF
      IF (DOHIST.GT.-9.5) THEN
         WRITE (LINE,1001) TSKNAM, LASTR
         CALL OHWRIT (LINE, UVDATA, JERR)
         IF (JERR.NE.0) DOHIST = -10.0
         END IF
C                                       open FG file
      CALL OFGINI (FGFILE, 'WRIT', FGROW, IERR)
      IF (IERR.NE.0) GO TO 980
      FGOPEN = 2
      IF (DOHIST.GT.0.0) THEN
         WRITE (LINE,1002) TSKNAM
         CALL OHWRIT (LINE, UVDATA, JERR)
         IF (JERR.NE.0) DOHIST = -10.0
         END IF
C                                       Set sort to unsorted
      COLS(1) = 0
      COLS(2) = 0
      DIM(1) = 2
      DIM(2) = 1
      CALL TABPUT (FGFILE, 'SORT', OOAINT, DIM, COLS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Copy input FG to output
      IF ((FGVERI.GT.0) .AND. (FGVERI.NE.VERS)) THEN
C                                       Create FGFILE object
         FIFILE = 'FG table was used to edit'
         PROBLM = FIFILE
         CALL CREATE (FIFILE, 'TABLE', IERR)
         IF (IERR.NE.0) GO TO 980
         FIOPEN = 1
         CALL IN2OBJ (UVDATA, NKEY1, OUTK1, OUTK1, FIFILE, IERR)
         IF (IERR.NE.0) GO TO 980
         DIM(1) = 1
         CALL OPUT (FIFILE, 'VER', TYPE, DIM, FGVERI, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         INEXT = 'FG'
         DIM(1) = 2
         CALL OPUT (FIFILE, 'TBLTYPE', OOACAR, DIM, DUMMY, INEXT, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL OFGINI (FIFILE, 'READ', FIROW, IERR)
         IF (IERR.NE.0) GO TO 980
         FIOPEN = 2
         CALL OGET (FIFILE, 'NROW', TYPE, DIM, NROWI, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         FIROW = 1
         NADD = 0
         DO 20 IROW = 1,NROWI
            PROBLM = FIFILE
            CALL OTABFG (FIFILE, 'READ', FIROW, FLGSOR, FLGSUB, FLGFQ,
     *         FGANTS, FLGTIM, FLGIF, FLGCHN, PFLAGS, FLGREA, IERR)
            IF (IERR.GT.0) GO TO 980
            IF (IERR.EQ.0) THEN
               PROBLM = FGFILE
               CALL OTABFG (FGFILE, 'WRIT', FGROW, FLGSOR, FLGSUB,
     *            FLGFQ, FGANTS, FLGTIM, FLGIF, FLGCHN, PFLAGS, FLGREA,
     *            IERR)
               IF (IERR.NE.0) GO TO 980
               NADD = NADD + 1
               END IF
 20         CONTINUE
         IF (NADD.GT.0) THEN
            WRITE (MSGTXT,1020) NADD, FGVERI
            CALL MSGWRT (3)
            END IF
         END IF
C                                       don't copy again
      FGVERI = -ABS(FGVERI)
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVDATA, FGVER(3), OOAINT, DIM, FGVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       input henceforth the new output
      CALL OPUT (UVDATA, FGVER(1), OOAINT, DIM, VERS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL OPUT (UVDATA, 'CALEDIT.FGVER', OOAINT, DIM, VERS, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 980
C                                       loop reading
      FCROW = 1
      NADD = 0
      DO 70 IROW = 1,LASTR
         PROBLM = FCFILE
         CALL OTABFC (FCFILE, 'READ', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *      FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.GT.0) GO TO 980
         IF (IERR.EQ.0) THEN
            PROBLM = FGFILE
C                                       all OPs are now general
            CALL EDANTS (ABS(FLGANT(2)), FGANTS(1), FGANTS(2))
            DO 60 I = 1,4
               PFLAGS(I) = FLGSTK(I:I).EQ.'1'
 60            CONTINUE
            IF ((FLGREA.EQ.TSKNAM(:5)//':date time') .OR.
     *         (FLGREA.EQ.' ')) FLGREA = REAZON
            CALL OTABFG (FGFILE, 'WRIT', FGROW, FLGSOR, FLGSUB, FLGFQ,
     *         FGANTS, FLGTIM, FLGIF, FLGCHN, PFLAGS, FLGREA, IERR)
            IF (IERR.NE.0) GO TO 980
            NADD = NADD + 1
            IF (DOHIST.GT.0.0) THEN
               CALL TODHMS (DTIMES(1), ITT(1))
               CALL TODHMS (DTIMES(2), ITT(5))
               WRITE (LINE,1010) TSKNAM, FLGOP, FGANTS(1), FGANTS(2),
     *            FLGIF(1), ITT, FLGSTK
               CALL OHWRIT (LINE, UVDATA, JERR)
               IF (JERR.NE.0) DOHIST = -10.0
               END IF
            END IF
 70      CONTINUE
      IF (NADD.GT.0) THEN
         IF (VERS.LE.0) THEN
            CALL OGET (FGFILE, 'VER', TYPE, DIM, VERS, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OPUT (UVDATA, FGVER(1), TYPE, DIM, VERS, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         CALL OGET (UVDATA, 'CALEDIT.FGVER', TYPE, DIM, I, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (I.LE.0) THEN
            CALL OPUT (UVDATA, 'CALEDIT.FGVER', TYPE, DIM, VERS,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         IF (DOHIST.GT.-9.5) THEN
            WRITE (LINE,1000) TSKNAM, VERS
            CALL OHWRIT (LINE, UVDATA, JERR)
            IF (JERR.NE.0) DOHIST = -10.0
            END IF
         WRITE (MSGTXT,1070) NADD, VERS
         CALL MSGWRT (3)
         END IF
      GO TO 990
C
 980  MSGTXT = 'EDFCAP: PROBLEM WITH ' // PROBLM
      CALL MSGWRT (7)
C
 990  IF (FGOPEN.EQ.2) CALL OTABFG (FGFILE, 'CLOS', FGROW, FLGSOR,
     *   FLGSUB, FLGFQ, FGANTS, FLGTIM, FLGIF, FLGCHN, PFLAGS, REAZON,
     *   JERR)
      IF (FGOPEN.GT.0) CALL TABDES (FGFILE, JERR)
C
      IF (FIOPEN.EQ.2) CALL OTABFG (FIFILE, 'CLOS', FIROW, FLGSOR,
     *   FLGSUB, FLGFQ, FGANTS, FLGTIM, FLGIF, FLGCHN, PFLAGS, REAZON,
     *   JERR)
C
 995  IF (FCOPEN.GT.0) CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM,
     *   FLGANT, FLGSOR, FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM,
     *   FLGOP, FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'FLAGVER =',I5,5X,'/ Output flag table version')
 1001 FORMAT (A6,'FLAGROW =',I5,5X,'/ Number rows added to flag table')
 1002 FORMAT (A6,'/Operation  Ants  IF',9X,'Time range',9X,'Stokes')
 1010 FORMAT (A6,'/ ',A8,I3.2,'-',I2.2,I4,2(I4.1,'/',2(I2.2,':'),I2.2),
     *  2X,A)
 1020 FORMAT ('EDFCAP: copied',I5,' rows from FG table version',I4)
 1070 FORMAT ('EDFCAP: added ',I5,' rows to   FG table version',I4)
      END
      SUBROUTINE EDFCDO (IERR)
C-----------------------------------------------------------------------
C   Reapplies FC table's contents
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Output:
C      IERR   I      Error code: 0 => all is still well
C                       > 0 => dies of unnatural causes
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   IROW, LASTR, ITYPE, I, INLANT, NFL, NFLT, FLGITS(2),
     *   POLOLD, IFOLD, INLIF, ITEMP(2)
      REAL      FLUXS(2)
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIFCPS.INC'
      LOGICAL   INUVFL, INLPOL, DONEIF(MAXIF,2)
C-----------------------------------------------------------------------
C                                       open table for read
      INUVFL = UVFLAG
      INLANT = ALLANT
      INLIF  = ALLIF
      ITEMP(1) = FGIFRS(1)
      ITEMP(2) = FGIFRS(2)
      INLPOL = ALLPOL
      NFLT = 0
      FLUXS(1) = -1.E20
      FLUXS(2) = 1.E20
      POLOLD = POLNOW
      IFOLD = IFNOW
      PLTIF = IFNOW
      PLTPOL = POLNOW
      IF (UVFLAG) THEN
         FLGMSG = .FALSE.
         UVFLAG = .FALSE.
         CALL OFCINI (FCFILE, 'READ', FLGNUM, LASTR, IERR)
         IF (IERR.NE.0) GO TO 980
         FLGNMX = FLGNUM
C                                       loop reading
         FCROW = 1
         DO 20 IROW = 1,LASTR
            CALL OTABFC (FCFILE, 'READ', FCROW, FLGTIM, FLGANT, FLGSOR,
     *         FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *         FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
            IF (IERR.GT.0) GO TO 980
            IF ((IERR.EQ.0) .AND. ((DIMIF.GT.1) .OR. (IFNOW.EQ.0) .OR.
     *         (IFNOW.EQ.FLGIF(1)) .OR. (FLGIF(1).EQ.0))) THEN
               ITYPE = 1
               DO 10 I = 1,DIMDAT-1
                  IF (LDTYPE.EQ.DTYPE(I)(:8)) ITYPE = I
 10               CONTINUE
               ALLANT = FLGANT(2)
               IF (FLGIF(2).EQ.0) THEN
                  ALLIF = 1
                  I = EIF - BIF + 1
                  CALL LFILL (I, .FALSE., DONEIF(BIF,1))
                  CALL LFILL (I, .FALSE., DONEIF(BIF,2))
               ELSE
                  ALLIF = 0
                  FGIFRS(1) = FLGIF(1)
                  FGIFRS(2) = FLGIF(2)
                  I = EIF - BIF + 1
                  CALL LFILL (I, .TRUE., DONEIF(BIF,1))
                  CALL LFILL (I, .TRUE., DONEIF(BIF,2))
                  I = FGIFRS(2) - FGIFRS(1) + 1
                  CALL LFILL (I, .FALSE., DONEIF(FGIFRS(1),1))
                  CALL LFILL (I, .FALSE., DONEIF(FGIFRS(1),2))
                  END IF
               ALLPOL = FLGSTK(1:2).EQ.'11'
               CALL EDTIMI (FLGTIM, FLGITS)
               DO 15 IFNOW = BIF,EIF
                  DO 14 POLNOW = 1,2
                     IF ((FLGSTK(POLNOW:POLNOW).EQ.'1') .AND.
     *                  (.NOT.DONEIF(IFNOW,POLNOW)) .AND.
     *                  ((ALLIF.EQ.1) .OR. ((IFNOW.GE.FLGIF(1)) .AND.
     *                  (IFNOW.LE.FLGIF(2))))) THEN
                        IF (ALLIF.EQ.1) THEN
                           I = EIF - BIF + 1
                           CALL LFILL (I, .TRUE., DONEIF(BIF,POLNOW))
                           IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                        DONEIF(BIF,3-POLNOW))
                        ELSE
                           I = FLGIF(2) - FLGIF(1) + 1
                           CALL LFILL (I, .TRUE.,
     *                        DONEIF(FLGIF(1),POLNOW))
                           IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                        DONEIF(FLGIF(1),3-POLNOW))
                           END IF
                        CALL EDFLAG ('FLAG', 0, FLGANT(1), ITYPE,
     *                     FLGITS, FLUXS, NFL, IERR)
                        IF (IERR.GT.0) GO TO 980
                        NFLT = NFLT + NFL
                        END IF
 14                  CONTINUE
 15               CONTINUE
               POLNOW = POLOLD
               IFNOW = IFOLD
               END IF
 20         CONTINUE
         FLGMSG = .TRUE.
C                                       close
         CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT,
     *      LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.GT.0) GO TO 980
         IF (NFLT.GT.0) THEN
            WRITE (MSGTXT,1020) NFLT
            CALL MSGWRT (3)
            END IF
      ELSE
         IERR = 0
         END IF
      GO TO 990
C
 980  MSGTXT = 'EDFCDO: ERROR WITH ' // FCFILE
      CALL MSGWRT (6)
C
 990  UVFLAG = INUVFL
      ALLANT = INLANT
      ALLIF  = INLIF
      FGIFRS(1) = ITEMP(1)
      FGIFRS(2) = ITEMP(2)
      ALLPOL = INLPOL
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('EDFCDO: flagged',I7,' points not previously flagged')
      END
      SUBROUTINE EDFCLI (IERR)
C-----------------------------------------------------------------------
C   lists an FC table's contents
C   Output:
C      IERR     I      Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   IROW, ITT(8), LLGNUM, LASTR, ANT1, ANT2, I, ITRIM
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIFCPS.INC'
C-----------------------------------------------------------------------

C                                       open table for read
      IF (UVFLAG) THEN
         CALL OFCINI (FCFILE, 'READ', FLGNUM, LASTR, IERR)
         IF (IERR.NE.0) GO TO 980
         FLGNMX = FLGNUM
         WRITE (MSGTXT,1000) LASTR, FLGNUM
         CALL MSGWRT (3)
         IF (DDTYPE.NE.'UV') THEN
            WRITE (MSGTXT,1005)
         ELSE
            WRITE (MSGTXT,1006)
            END IF
         IF (LASTR.GT.0) CALL MSGWRT (3)
C                                       loop reading
         FCROW = 1
         LLGNUM = 0
         DO 20 IROW = 1,LASTR
            CALL  OTABFC (FCFILE, 'READ', FCROW, FLGTIM, FLGANT, FLGSOR,
     *         FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *         FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
            IF (IERR.GT.0) GO TO 980
            IF ((IERR.EQ.0) .AND. (FLGNUM.NE.LLGNUM)) THEN
               LLGNUM = FLGNUM
               CALL TODHMS (DTIMES(1), ITT(1))
               CALL TODHMS (DTIMES(2), ITT(5))
               CALL EDANTS (ABS(FLGANT(2)), ANT1, ANT2)
               IF (DDTYPE.NE.'UV') THEN
                  IF (ANT1.GT.0) THEN
                     WRITE (MSGTXT,1010) FLGNUM, IROW, FLGOP, ANT1,
     *                  FLGIF, ITT, FLGSTK
                  ELSE
                     WRITE (MSGTXT,1011) FLGNUM, IROW, FLGOP, FLGIF,
     *                  ITT, FLGSTK
                     END IF
               ELSE
                  IF ((ANT1.GT.0) .AND. (ANT2.GT.0)) THEN
                     WRITE (MSGTXT,1015) FLGNUM, IROW, FLGOP, ANT1,
     *                  ANT2, FLGIF, ITT, FLGSTK
                  ELSE IF (ANT1.GT.0) THEN
                     WRITE (MSGTXT,1016) FLGNUM, IROW, FLGOP, ANT1,
     *                  FLGIF, ITT, FLGSTK
                  ELSE
                     WRITE (MSGTXT,1017) FLGNUM, IROW, FLGOP, FLGIF,
     *                  ITT, FLGSTK
                     END IF
                  END IF
               CALL MSGWRT (3)
C                                       fluxes used
               IF (FLGOP(:3).NE.'TIM') THEN
                  IF (DDTYPE.NE.'UV') THEN
                     WRITE (MSGTXT,1020) DFLUXS, LDTYPE
                  ELSE
                     WRITE (MSGTXT,1021) DFLUXS, LDTYPE
                     END IF
                  CALL MSGWRT (3)
                  END IF
C                                       reason
               IF ((FLGREA.NE.TSKNAM(:5)//':date time') .AND.
     *            (FLGREA.NE.' ')) THEN
                  MSGTXT = ' '
                  I = 29
                  IF (DDTYPE.NE.'UV') I = 28
                  MSGTXT(I:) = 'Reason = ''' // FLGREA(:ITRIM(FLGREA))
     *               // ''''
                  CALL MSGWRT (3)
                  END IF
               END IF
 20         CONTINUE
C                                       close
         CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT,
     *      LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.GT.0) GO TO 980
      ELSE
         IERR = 0
         END IF
      GO TO 999
C
 980  MSGTXT = 'EDFCLI: ERROR WITH ' // FCFILE
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FC file with',I5,' rows and',I5,' flag commands:')
 1005 FORMAT (' FC#  Row Operation Ant   IFs',10X,'Time range',8X,
     *   'Stokes')
 1006 FORMAT (' FC#  Row Operation Ants   IFs',10X,'Time range',8X,
     *   'Stokes')
 1010 FORMAT (I4,I5,2X,A8,2I4,I3,2(I4.1,'/',2(I2.2,':'),I2.2),2X,A)
 1011 FORMAT (I4,I5,2X,A8,'   *',I4,I3,2(I4.1,'/',2(I2.2,':'),I2.2),
     *   2X,A)
 1015 FORMAT (I4,I5,1X,A8,I3.2,'-',I2.2,I4,I3,2(I4.1,'/',2(I2.2,':'),
     *  I2.2),1X,A)
 1016 FORMAT (I4,I5,1X,A8,I3.2,'- *',I4,I3,2(I4.1,'/',2(I2.2,':'),I2.2),
     *  1X,A)
 1017 FORMAT (I4,I5,1X,A8,' * - *',I4,I3,2(I4.1,'/',2(I2.2,':'),I2.2),
     *  1X,A)
 1020 FORMAT (27X,2(1PE13.4),2X,A)
 1021 FORMAT (28X,2(1PE13.4),1X,A)
      END
      SUBROUTINE EDFCUN (ENTRY, IERR)
C-----------------------------------------------------------------------
C   Undoes one entry in the FC table: used only for UVFLAG True.
C   Inputs:
C      IGS     I(4)   Graphics plane of data, extra data, flagged, top
C      ENTRY   I(2)   Entry number range to undo
C   Output:
C      IERR    I      Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C-----------------------------------------------------------------------
      INTEGER   ENTRY(2), IERR
C
      INTEGER   IROW, LLGNUM, LASTR, ITYPE, I, DIM(7), NSKIP, OFCROW,
     *   OFLGNM, MSGSAV, INLANT, FLGITS(2), NFL, NFLT, FLITIM(2),
     *   POLOLD, IFOLD, INLIF, ITEMP(2)
      CHARACTER FCTEMP*32, PROBLM*32, CDUMMY*1
      REAL      FLUXS(2)
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIFCPS.INC'
      LOGICAL   EXIST, INPOL, DONEIF(MAXIF,2), DOCOPY
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       open table for read
      INLANT = ALLANT
      INLIF  = ALLIF
      ITEMP(1) = FGIFRS(1)
      ITEMP(2) = FGIFRS(2)
      INPOL  = ALLPOL
      FLUXS(1) = -1.E20
      FLUXS(2) = 1.E20
      NFLT = 0
      NSKIP = 0
      POLOLD = POLNOW
      IFOLD = IFNOW
      IF (UVFLAG) THEN
         FLGMSG = .FALSE.
         PROBLM = FCFILE
         CALL OFCINI (FCFILE, 'READ', FLGNUM, LASTR, IERR)
         IF (IERR.NE.0) GO TO 980
         IF ((ENTRY(2).LT.1) .OR. (ENTRY(1).GT.FLGNUM) .OR.
     *      (ENTRY(2).LT.ENTRY(1))) THEN
            NSKIP = -1
            WRITE (MSGTXT,1000) ENTRY, FLGNUM
            IF (FLGNUM.EQ.0) MSGTXT = 'EDFCUN: No flags left to undo'
            CALL MSGWRT (7)
C                                       loop reading: copy to scratch
         ELSE
C                                       Create FCTEMP object
            FCTEMP = 'Temporary FC file for undo'
            PROBLM = FCTEMP
            CALL CREATE (FCTEMP, 'TABLE', IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Copy the object part
            CALL TBCOPY (FCFILE, FCTEMP, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       new version
            DIM(1) = 1
            DIM(2) = 1
            I = 2
            CALL OPUT (FCTEMP, 'VER', OOAINT, DIM, I, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Make sure output doesn't exist
            CALL OBFEXS (FCTEMP, EXIST, IERR)
            IERR = 0
            IF (EXIST) THEN
               CALL TABRMV (FCTEMP, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
C                                       open file
            CALL OFCINI (FCTEMP, 'WRIT', OFLGNM, OFCROW, IERR)
            IF (IERR.NE.0) GO TO 980
            OFCROW = 1
            FCROW = 1
            LLGNUM = 0
            OFLGNM = 0
            NSKIP = 0
            DO 20 IROW = 1,LASTR
               PROBLM = FCFILE
               CALL  OTABFC (FCFILE, 'READ', FCROW, FLGTIM, FLGANT,
     *            FLGSOR, FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM,
     *            FLGOP, FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
               IF (IERR.GT.0) GO TO 980
C                                       check for unavailable data
               DOCOPY = (FLGNUM.LT.ENTRY(1)) .OR. (FLGNUM.GT.ENTRY(2))
C                                       copy to output
               IF (DOCOPY) THEN
                  IF (LLGNUM.NE.FLGNUM) OFLGNM = OFLGNM + 1
                  LLGNUM = FLGNUM
                  PROBLM = FCTEMP
                  CALL  OTABFC (FCTEMP, 'WRIT', OFCROW, FLGTIM, FLGANT,
     *               FLGSOR, FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ,
     *               OFLGNM, FLGOP, FLGIT, LDTYPE, DTIMES, DFLUXS,
     *               FLGREA, IERR)
                  IF (IERR.GT.0) GO TO 980
                  FLGNMX = OFLGNM
C                                       skip this one
               ELSE
                  NSKIP = NSKIP + 1
                  IF (NSKIP.EQ.1) CALL COPY (2, FLGIT, FLITIM)
                  FLITIM(1) = MIN (FLITIM(1), FLGIT(1))
                  FLITIM(2) = MAX (FLITIM(2), FLGIT(2))
C                                       do unflaging
                  ITYPE = 1
                  DO 10 I = 1,DIMDAT-1
                     IF (LDTYPE.EQ.DTYPE(I)(:8)) ITYPE = I
 10                  CONTINUE
                  ALLANT = FLGANT(2)
                  IF (FLGIF(2).EQ.0) THEN
                     ALLIF = 1
                     I = EIF - BIF + 1
                     CALL LFILL (I, .FALSE., DONEIF(BIF,1))
                     CALL LFILL (I, .FALSE., DONEIF(BIF,2))
                  ELSE
                     ALLIF = 0
                     FGIFRS(1) = FLGIF(1)
                     FGIFRS(2) = FLGIF(2)
                     I = EIF - BIF + 1
                     CALL LFILL (I, .TRUE., DONEIF(BIF,1))
                     CALL LFILL (I, .TRUE., DONEIF(BIF,2))
                     I = FGIFRS(2) - FGIFRS(1) + 1
                     CALL LFILL (I, .FALSE., DONEIF(FGIFRS(1),1))
                     CALL LFILL (I, .FALSE., DONEIF(FGIFRS(1),2))
                     END IF
                  ALLPOL = FLGSTK.EQ.'1111'
                  CALL EDTIMI (FLGTIM, FLGITS)
                  PLTIF = IFOLD
                  PLTPOL = POLOLD
                  DO 15 IFNOW = BIF,EIF
                     DO 14 POLNOW = 1,2
                        IF ((FLGSTK(POLNOW:POLNOW).EQ.'1') .AND.
     *                     (.NOT.DONEIF(IFNOW,POLNOW)) .AND.
     *                     ((ALLIF.EQ.1) .OR. ((IFNOW.GE.FLGIF(1)) .AND.
     *                     (IFNOW.LE.FLGIF(2))))) THEN
                           IF (ALLIF.EQ.1) THEN
                              I = EIF - BIF + 1
                              CALL LFILL (I, .TRUE., DONEIF(BIF,POLNOW))
                              IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                           DONEIF(BIF,3-POLNOW))
                           ELSE
                              I = FLGIF(2) - FLGIF(1) + 1
                              CALL LFILL (I, .TRUE.,
     *                           DONEIF(FLGIF(1),POLNOW))
                              IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                           DONEIF(FLGIF(1),3-POLNOW))
                              END IF
                           CALL EDFLAG ('UNFL', 0, FLGANT(1), ITYPE,
     *                        FLGITS, FLUXS, NFL, IERR)
                           IF (IERR.GT.0) GO TO 980
                           NFLT = NFLT + NFL
                           END IF
 14                     CONTINUE
 15                  CONTINUE
                  POLNOW = POLOLD
                  IFNOW = IFOLD
                  END IF
 20            CONTINUE
            END IF
         FLGMSG = .TRUE.
         IF (NSKIP.EQ.0) GO TO 900
C                                       close input FC
         PROBLM = FCFILE
         CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT,
     *      LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.GT.0) GO TO 980
C                                       do move 2 -> 1
         IF (NSKIP.GT.0) THEN
            FLAGED = .TRUE.
            PROBLM = FCTEMP
            CALL OTABFC (FCTEMP, 'CLOS', OFCROW, FLGTIM, FLGANT,
     *         FLGSOR, FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, OFLGNM,
     *         FLGOP, FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
            IF (IERR.GT.0) GO TO 980
            MSGSAV = MSGSUP
            MSGSUP = 31990
            CALL TBLCOP (FCTEMP, FCFILE, IERR)
            MSGSUP = MSGSAV
            IF (IERR.NE.0) GO TO 980
            CALL TABZAP (FCTEMP, IERR)
            IF (IERR.NE.0) GO TO 980
            WRITE (MSGTXT,1020) NFLT, FLITIM
            CALL MSGWRT (3)
            WRITE (MSGTXT,1021) NSKIP
            CALL MSGWRT (3)
            END IF
      ELSE
         IERR = 0
         END IF
      GO TO 990
C                                       failed to need copy
 900  PROBLM = FCFILE
      CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *   FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT,
     *   LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
      IF (IERR.GT.0) GO TO 980
      FLGNMX = FLGNUM
      PROBLM = FCTEMP
      CALL  OTABFC (FCTEMP, 'CLOS', OFCROW, FLGTIM, FLGANT, FLGSOR,
     *   FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, OFLGNM, FLGOP, FLGIT,
     *   LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
      IF (IERR.GT.0) GO TO 980
      CALL TABZAP (FCTEMP, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 990
C
 980  MSGTXT = 'EDFCUN: ERROR WITH ' // PROBLM
      CALL MSGWRT (6)
C
 990  ALLANT = INLANT
      ALLIF  = INLIF
      FGIFRS(1) = ITEMP(1)
      FGIFRS(2) = ITEMP(2)
      ALLPOL = INPOL
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EDFCUN: FLAG COMMANDS',2I5,' OUTSIDE RANGE 1 -',I5)
 1020 FORMAT ('EDFCUN: restored',I6,' points in T range',2I7)
 1021 FORMAT ('EDFCUN: removed',I6,' records from flag command table')
      END
      SUBROUTINE EDFGAP (UVDATA, IERR)
C-----------------------------------------------------------------------
C   Applies a uv data set FG table to table data now in core
C   Inputs:
C      UVDATA   C*(*)   UV master data set being flagged
C   Output:
C      IERR     I       Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C   UV Editor common is used: UVFLAG, ...
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      INTEGER   IERR
C
      INTEGER   NKEY1
      PARAMETER (NKEY1 = 4)
      CHARACTER FGFILE*32, CDUMMY*1, OUTK1(NKEY1)*8, FGVER*32, INEXT*2,
     *   PROBLM*32, REAZON*24, FLGREA*24
      LONGINT   LPTR, JPTR
      INTEGER   DIM(7), TYPE, VERS, FGOPEN, JERR, LASTR, DUMMY, FGROW,
     *   FGANTS(2), NADD, LF1, LF2, KF1, KF2, LR, LT, IPOL, LF, FLGSOR,
     *   FLGCHN(2), FLGIF(2), FLGSUB, FLGFQ, FGREC
      REAL      FLGTIM(2)
      LOGICAL   PFLAGS(4), NOANT, EXISTS
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Adverbs to copy from UVDATA
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
      DATA FGVER /'IN_FGVER'/
C-----------------------------------------------------------------------
      FGOPEN = 0
C                                       Create FGFILE object
      FGFILE = 'FG table to be used to edit'
      PROBLM = FGFILE
      CALL CREATE (FGFILE, 'TABLE', IERR)
      IF (IERR.NE.0) GO TO 980
      FGOPEN = 1
C                                       copy basic adverbs
      PROBLM = UVDATA
      CALL IN2OBJ (UVDATA, NKEY1, OUTK1, OUTK1, FGFILE, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       set version, type
      CALL OGET (UVDATA, FGVER, TYPE, DIM, VERS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (VERS.LT.0) GO TO 990
      MSGTXT = 'Applying FG table to the data'
      CALL MSGWRT (2)
C                                       actually apply the FG table
      PROBLM = FGFILE
      CALL OPUT (FGFILE, 'VER', TYPE, DIM, VERS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      INEXT = 'FG'
      DIM(1) = 2
      CALL OPUT (FGFILE, 'TBLTYPE', OOACAR, DIM, DUMMY, INEXT, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       does it exist
      CALL TABEXI (FGFILE, EXISTS, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (.NOT.EXISTS) GO TO 990
C                                       open FG file
      CALL OFGINI (FGFILE, 'READ', FGROW, IERR)
      IF (IERR.NE.0) GO TO 980
      FGOPEN = 2
      CALL OGET (FGFILE, 'NROW', TYPE, DIM, LASTR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       set IF loop range
      IF (ALLIN) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
C                                       loop reading
      NADD = 0
      DO 100 FGREC = 1,LASTR
         PROBLM = FGFILE
         FGROW = FGREC
         CALL OTABFG (FGFILE, 'READ', FGROW, FLGSOR, FLGSUB, FLGFQ,
     *      FGANTS, FLGTIM, FLGIF, FLGCHN, PFLAGS, FLGREA, IERR)
         IF (IERR.GT.0) GO TO 980
         IF (FLGIF(1).LE.0) FLGIF(1) = BIF
         IF (FLGIF(2).LE.0) FLGIF(2) = EIF
         IF (FLGCHN(1).LE.0) FLGCHN(1) = BCHAN
         IF (FLGCHN(2).LE.0) FLGCHN(2) = ECHAN
C                                       does it apply?
         IF ((IERR.EQ.0) .AND. ((FGANTS(1).EQ.0) .OR. (FGANTS(2).EQ.0))
     *      .AND. ((FLGSUB.EQ.SUBARR) .OR. (FLGSUB.LE.0)) .AND.
     *      (FLGIF(1).LE.LF2) .AND. (FLGIF(2).GE.LF1) .AND.
     *      ((FLGFQ.LE.0) .OR. (FRQSEL.LE.0) .OR. (FRQSEL.EQ.FLGFQ))
     *      .AND. (FLGTIM(1).LT.TEND) .AND. (FLGTIM(2).GT.TSTART) .AND.
     *      (FLGCHN(1).LE.BCHAN) .AND. (FLGCHN(2).GE.ECHAN) .AND.
     *      ((PFLAGS(1)) .OR. ((PFLAGS(2)) .AND. (POLMAX.GT.1)))) THEN
            NOANT = (FGANTS(1).EQ.0) .AND. (FGANTS(2).EQ.0)
            KF1 = MAX (LF1, FLGIF(1))
            KF2 = MIN (LF2, FLGIF(2))
            DO 50 LR = 1,MAXREC
               LPTR = DPTR + (LR - 1) * DIMREC
               LT = EDCORI(LPTR+1)
               IF (FLGTIM(2).LT.TIMES(EDIPTR+LT)) GO TO 100
C                                       antenna & time match
               IF (((NOANT) .OR. (EDCORI(LPTR).EQ.FGANTS(1)) .OR.
     *            (EDCORI(LPTR).EQ.FGANTS(2))) .AND.
     *            (FLGTIM(1).LE.TIMES(EDIPTR+LT)) .AND. ((FLGSOR.LE.0)
     *            .OR. (EDCORI(SUPTR+LT-1).LE.0) .OR.
     *            (EDCORI(SUPTR+LT-1).EQ.FLGSOR))) THEN
                  DO 40 IPOL = 1,POLMAX
                     IF (PFLAGS(IPOL)) THEN
                        JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
     *                     - LF1
                        DO 30 LF = KF1,KF2
                           IF (EDCORE(JPTR+LF).GT.0.0) THEN
                              NADD = NADD + 1
                              EDCORE(JPTR+LF) = 0.0
                              END IF
 30                        CONTINUE
                        END IF
 40                  CONTINUE
                  END IF
 50            CONTINUE
            END IF
 100     CONTINUE
      IF (NADD.GT.0) THEN
         WRITE (MSGTXT,1100) NADD, VERS
         CALL MSGWRT (3)
         WASFLG = MAX (0, WASFLG) + NADD
         END IF
      IERR = MAX (0, IERR)
      GO TO 990
C
 980  MSGTXT = 'EDFGAP: PROBLEM WITH ' // PROBLM
      CALL MSGWRT (7)
C
 990  IF (FGOPEN.EQ.2) CALL OTABFG (FGFILE, 'CLOS', FGROW, FLGSOR,
     *   FLGSUB, FLGFQ, FGANTS, FLGTIM, FLGIF, FLGCHN, PFLAGS, REAZON,
     *   JERR)
      IF (FGOPEN.GT.0) CALL TABDES (FGFILE, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('EDFGAP: flagged',I9,' samples using uvdata flag table',
     *   I2)
      END
      SUBROUTINE EDFIND (MODE, WTYPE, IAN, TYPE, TVXY, X, NPIF, IT, IR,
     *   IERR)
C-----------------------------------------------------------------------
C   find the nearest plotted sample to the TV cursor in TVXY
C   Input:
C      MODE   I      0 -> nearest to TVXY(1), applying TVXY(2) to select
C                    only one; 1 -> find lowest IT to which TVXY(1)
C                    applies, 2 -> find highest IT to which TVXY(1)
C                    applies, 3 -> nearest to TVXY, 4 -> nearest > TVXY,
C                    5 -> nearest < TVXY(1)
C      WTYPE  I      -1 -> only flagged, 0 don't care, 1 only good
C      IAN    I      Antenna number
C      TYPE   I      Desired data type
C      TVXY   R(2)   TV cursor position
C   Output:
C      X      R(3)   Value on T, data, weight axes
C      NPIF   I(2)   Polarization number, IF number of nearest
C      IT     I      Time index of nearest
C      IR     I      Record number of nearest
C      IERR   I      Error code: > 0 serious, < 0 none found
C-----------------------------------------------------------------------
      INTEGER   MODE, WTYPE, IAN, TYPE, NPIF(2), IT, IR, IERR
      REAL      TVXY(2), X(3)
C
      LONGINT   LPTR, JPTR, IPTR
      INTEGER   J, JIN, LR1, LR2, LR, IT2, IR2, IT1, IR1, ITHI, ITLO,
     *   NL, ITL(5000), IRL(5000), NLU, I, LMODE, IP1, IP2, IF1, IF2,
     *   IPP, IFF, IFF1, IFF2, IPP1, IPP2, IPL(5000), IFL(5000)
      REAL      V, V2, V1, W2, W1, XS, VTL(5000), WTL(5000), YS, VM, TT
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      COMMON /EDISCR/ ITL, IRL, IPL, IFL, VTL, WTL
C-----------------------------------------------------------------------
      IERR = 0
      XS = REAL (XYPLOT(3,1) - XYPLOT(1,1) - 2*LEDG) / MAX (1.0,
     *   REAL (TIME2 - TIME1))
      X(1) = (TVXY(1) - XYPLOT(1,1) - LEDG) / XS + TIME1
      IF (XS.GE.1.0) THEN
         LMODE = MAX (3, MODE)
         IF (MODE.EQ.0) LMODE = 0
         ITHI = X(1) + 0.999
         ITLO = X(1)
      ELSE
         LMODE = MODE
         IF (MODE.GT.3) LMODE = 6 - MODE
         ITHI = X(1) + 0.499 / XS
         ITLO = X(1) - 0.500 / XS
         END IF
      JIN = X(1) + 0.001
      IF (POLNOW.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (IFNOW.EQ.0) THEN
         IF1 = BIF
         IF2 = EIF
      ELSE IF (DIMIF.GT.1) THEN
         IF1 = IFNOW
         IF2 = IFNOW
      ELSE
         IF1 = BIF
         IF2 = BIF
         END IF
C     IPTR = DIMPRM + (POLNOW-1) * DIMIF * DIMDAT
C     IF (DIMIF.GT.1) IPTR = IPTR + MAX (BIF, IFNOW) - BIF
C     JPTR = IPTR + TYPE * DIMIF
C                                       find a sample up
      NL = 0
      J = MAX (0, JIN - 1)
      IR2 = 0
      IT2 = 0
 10   J = J + 1
      IF (J.LT.TIMEM) THEN
         LR1 = EDCORI(PPTR+J-1)
         LR2 = EDCORI(PPTR+J) - 1
         IF (LR2.GE.LR1) THEN
            DO 20 LR = LR1,LR2
               LPTR = DPTR + (LR - 1) * DIMREC
               IF (IAN.EQ.EDCORI(LPTR)) THEN
                  DO 15 IFF = IF1,IF2
                  DO 15 IPP = IP1,IP2
                     IPTR = DIMPRM + (IPP-1) * DIMIF * DIMDAT + IFF -
     *                  BIF + LPTR
                     JPTR = IPTR + TYPE * DIMIF
                     IF ((EDCORE(IPTR).NE.0.0) .AND.
     *                  (WTYPE*EDCORE(IPTR).GE.0.0)) THEN
                        V = EDCORE(JPTR)
                        IF (V.NE.FBLANK) THEN
                           IF ((LMODE.GT.0) .AND. (LMODE.LT.3) .AND.
     *                        (J.GT.ITHI) .AND. (IT2.NE.0)) GO TO 40
                           V2 = V
                           IT2 = J
                           IR2 = LR
                           W2 = EDCORE(IPTR)
                           IPP2 = IPP
                           IFF2 = IFF
                           IF ((IT2.GE.X(1)) .AND. (LMODE.GE.3))
     *                        GO TO 40
                           IF (LMODE.EQ.0) THEN
                              IF ((NL.GT.0) .AND. (J.GT.ITHI+4))
     *                           GO TO 40
                              NL = NL + 1
                              VTL(NL) = V2
                              WTL(NL) = W2
                              IRL(NL) = IR2
                              ITL(NL) = IT2
                              IFL(NL) = IFF
                              IPL(NL) = IPP
                              IF ((NL.GT.0) .AND. (J.GT.ITHI+4))
     *                           GO TO 40
                              IF (NL.EQ.2500) GO TO 40
                              END IF
                           END IF
                        END IF
 15                  CONTINUE
                  END IF
 20            CONTINUE
            END IF
         GO TO 10
         END IF
C                                       find a sample down
 40   J = JIN + 1
      J = MIN (J, TIMEM)
      NLU = NL
      IR1 = 0
      IT1 = 0
 50   J = J - 1
      IF (J.GE.1) THEN
         LR1 = EDCORI(PPTR+J-1)
         LR2 = EDCORI(PPTR+J) - 1
         IF (LR2.GE.LR1) THEN
            DO 60 LR = LR1,LR2
               LPTR = DPTR + (LR - 1) * DIMREC
               IF (IAN.EQ.EDCORI(LPTR)) THEN
                  DO 55 IFF = IF1,IF2
                  DO 55 IPP = IP1,IP2
                     IPTR = DIMPRM + (IPP-1) * DIMIF * DIMDAT + IFF -
     *                  BIF + LPTR
                     JPTR = IPTR + TYPE * DIMIF
                     IF ((EDCORE(IPTR).NE.0.0) .AND.
     *                  (WTYPE*EDCORE(IPTR).GE.0.0)) THEN
                        V = EDCORE(JPTR)
                        IF (V.NE.FBLANK) THEN
                           IF ((LMODE.GT.0) .AND. (LMODE.LT.3) .AND.
     *                        (J.LT.ITLO) .AND. (IT1.NE.0)) GO TO 90
                           V1 = V
                           IT1 = J
                           IR1 = LR
                           W1 = EDCORE(IPTR)
                           IFF1 = IFF
                           IPP1 = IPP
                           IF ((IT1.LE.X(1)) .AND. (LMODE.GE.3))
     *                        GO TO 90
                           IF (LMODE.EQ.0) THEN
                              IF ((NL.GT.NLU) .AND. (J.LT.ITLO-4))
     *                           GO TO 90
                              NL = NL + 1
                              VTL(NL) = V1
                              WTL(NL) = W1
                              IRL(NL) = IR1
                              ITL(NL) = IT1
                              IFL(NL) = IFF
                              IPL(NL) = IPP
                              IF ((NL.GT.NLU) .AND. (J.LT.ITLO-4))
     *                           GO TO 90
                              IF (NL.EQ.5000) GO TO 90
                              END IF
                           END IF
                        END IF
 55                  CONTINUE
                  END IF
 60            CONTINUE
            END IF
         GO TO 50
         END IF
C                                       get the answer
 90   J = 0
      IF ((LMODE.EQ.1) .OR. (LMODE.EQ.5)) THEN
         IF (IT1.GT.0) J = 1
      ELSE IF ((LMODE.EQ.2) .OR. (LMODE.EQ.4)) THEN
         IF (IT2.GT.0) J = 2
      ELSE IF (LMODE.EQ.3) THEN
         IF ((IT1.GT.0) .AND. (IT2.GT.0)) THEN
            IF (ABS(IT1-X(1)).LT.ABS(IT2-X(1))) THEN
               J = 1
            ELSE
               J = 2
               END IF
         ELSE IF (IT1.GT.0) THEN
            J = 1
         ELSE IF (IT2.GT.0) THEN
            J = 2
            END IF
      ELSE IF (LMODE.EQ.0) THEN
         J = 0
         YS = REAL (XYPLOT(4,1) - XYPLOT(2,1) - 2*LEDG) / (APIXR(2,1) -
     *      APIXR(1,1))
         V = (TVXY(2) - XYPLOT(2,1) - LEDG) / YS + APIXR(1,1)
C                                       Require accuracy in cursor
C                                       position
         VM = 3.9
         DO 95 I = 1,NL
            V = (VTL(I) - APIXR(1,1)) * YS + XYPLOT(2,1) + LEDG
            TT = (ITL(I) - TIME1) * XS + XYPLOT(1,1) + LEDG
            V = (V - TVXY(2)) ** 2 + (TT - TVXY(1)) ** 2
            IF (V.LT.VM) THEN
               J = I
               VM = V
               END IF
 95         CONTINUE
         IF (J.GT.0) THEN
            IT1 = ITL(J)
            IR1 = IRL(J)
            V1  = VTL(J)
            W1 =  WTL(J)
            IFF1 = IFL(J)
            IPP1 = IPL(J)
            J = 1
            END IF
         END IF
      IF (J.EQ.1) THEN
         IT = IT1
         IR = IR1
         NPIF(1) = IPP1
         NPIF(2) = IFF1
         X(1) = IT1
         X(2) = V1
         X(3) = W1
      ELSE IF (J.EQ.2) THEN
         IT = IT2
         IR = IR2
         NPIF(1) = IPP2
         NPIF(2) = IFF2
         X(1) = IT2
         X(2) = V2
         X(3) = W2
      ELSE
         IERR = -1
         IT = JIN
         IR = 0
         NPIF(1) = 0
         NPIF(2) = 0
         X(1) = IT
         X(2) = 0.0
         X(3) = 0.0
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDFLAG (OP, FOP, IAN, TYPE, FLTIMS, FLFLUX, NFL, IERR)
C-----------------------------------------------------------------------
C   flag samples in specified range, remove them from plot and toplot,
C   put them on flagged plot (if any).  Can also unflag data in core.
C   Handles FC table for UV data flagging only, not unflagging.
C   Inputs:
C      OP       C*4    'UNFL' => unflag, else flag
C                      'FORC' make a new flag table entry even if
C                      already flagged.
C      FOP      I      Type of flag - 1 time, 2 timerange, 3 below,
C                      4 above, 5 area, 6 point; < 0 => do NOT raise
C                      the FC number
C      IAN      I      Antenna to flag (if not ALLANT), main antenna in
C                      plots in any case
C      TYPE     I      Data type to check value range
C      FLTIMS   I(2)   Time array indices of flag window
C      FLFLUX   R(2)   Value range to flag
C   Output:
C      NFL      I      Number points flagged/unflagged
C      IERR     I      Error code: 0 -> okay even if no points flagged
C   POLNOW and IFNOW must have real values before calling this routine.
C   The values of 0 => all are not handled inside EDFLAG.
C   2009-01-31: changed to require below and above to send in their
C   samples one at a time like area.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   FOP, IAN, TYPE, FLTIMS(2), NFL, IERR
      REAL      FLFLUX(2)
C
      LONGINT   LPTR, IPTR, JPTR
      INTEGER   LR, LR1, LR2, LA, LT, LF, LF1, LF2, LF0, IOP, ITP,
     *   IROUND, I, JJPT, JJPT2, TIME(3), DATE(3), LPP, LPP1, LPP2,
     *   NSORL, SORL(100), ISORL
      CHARACTER FLGOPS(6)*8, STFLAG(3)*4, TTIME(2)*12
      LOGICAL   DOIT, EDOANT, BLNKOK, DOE, DOE2, FORCE
      REAL      V, SGN, V2, PRFLUX(2), WT, W1, W2, AMP, LLFLUX(2)
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'EDIFCPS.INC'
      DATA FLGOPS / 'TIME', 'TIMRANGE', 'BELOW', 'ABOVE', 'AREA',
     *   'POINT'/
      DATA STFLAG /'1111', '1011','0111'/
C-----------------------------------------------------------------------
C                                       FC table
      SGN = -1.0
      IF (OP.EQ.'UNFL') SGN = 1.0
      DOIT = (UVFLAG) .AND. (OP.NE.'UNFL')
      FORCE = OP.EQ.'FORC'
      BLNKOK = (OP.EQ.'UNFL') .AND. (FLFLUX(1).LT.-1.E12) .AND.
     *   (FLFLUX(2).GT.1.E12)
      DOE = DOERRB
      DOE2 = DOERRB .AND. DOCOMP
      W1 = 0.0
      W2 = 0.0
C                                       set general parameters
      IF (FLTIMS(1).EQ.FLTIMS(2)) THEN
         IOP = 1
      ELSE IF ((FLFLUX(1).LT.-1.E10) .AND. (FLFLUX(2).GT.1.E10)) THEN
         IOP = 2
      ELSE IF (FLFLUX(1).LT.-1.E10) THEN
         IOP = 3
      ELSE IF (FLFLUX(2).GT.1.E10) THEN
         IOP = 4
      ELSE
         IOP = 5
         END IF
      IF ((FOP.NE.0) .AND. (ABS(FOP).LE.6)) IOP = ABS (FOP)
      IF (ALLIF.EQ.1) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE IF (ALLIF.EQ.0) THEN
         LF1 = FGIFRS(1)
         LF2 = FGIFRS(2)
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
      LLFLUX(1) = -1.E20
      LLFLUX(2) = 1.E20
C                                       Do FC table
      IF (DOIT) THEN
C                                       open table for write
         CALL OFCINI (FCFILE, 'WRIT', FLGNUM, FCROW, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (FOP.GE.0) FLGNUM = FLGNUM + 1
         FLGNMX = FLGNUM
         FCROW = FCROW + 1
         FLGOP = FLGOPS(IOP)
         FLGIF(2) = IFNOW
         IF (ALLIF.EQ.1) THEN
            FLGIF(1) = 0
            FLGIF(2) = 0
         ELSE IF (ALLIF.EQ.0) THEN
            FLGIF(1) = FGIFRS(1)
            FLGIF(2) = FGIFRS(2)
         ELSE
            FLGIF(1) = IFNOW
            FLGIF(2) = IFNOW
            END IF
         FLGANT(2) = ALLANT
         FLGSOR = 0
         FLGCHN(1) = 0
         FLGCHN(2) = 0
         FLGSUB = SUBARR
         FLGFQ = FRQSEL
         CALL EDTIMX (FLTIMS, DTIMES)
         DFLUXS(1) = FLFLUX(1)
         DFLUXS(2) = FLFLUX(2)
         LDTYPE = DTYPE(LTYPE)(:8)
         FLGIT(1) = FLTIMS(1)
         FLGIT(2) = FLTIMS(2)
         DO 10 LF = LF1,LF2
            IFWASN(LF) = .TRUE.
 10         CONTINUE
         FLGREA = REASON
         IF (FLGREA.EQ.' ') THEN
            CALL ZTIME (TIME)
            CALL ZDATE (DATE)
            DATE(1) = -DATE(1)
            CALL TIMDAT (TIME, DATE, TTIME(2), TTIME)
            FLGREA = TSKNAM // TTIME(1)(:9) // ' ' // TTIME(2)(:8)
            END IF
C                                       can do it with one write
C        IF ((IOP.NE.3) .AND. (IOP.NE.4)) THEN
         FLGANT(1) = IAN
         IF (ALLANT.EQ.0) FLGANT(1) = 0
         FLGSTK = STFLAG(POLNOW+1)
         IF ((ALLPOL) .OR. (POLMAX.EQ.1)) FLGSTK = STFLAG(1)
         CALL EDTIMX (FLGIT, FLGTIM)
C                                       Source id
         IF (ALLSOR) THEN
            NSORL = 1
            SORL(1) = 0
         ELSE
            NSORL = 100
            CALL EDSORL (FLGIT, NSORL, SORL)
            END IF
         DO 15 ISORL = 1,NSORL
            FLGSOR = SORL(ISORL)
            CALL OTABFC (FCFILE, 'WRIT', FCROW, FLGTIM, FLGANT,
     *         FLGSOR, FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM,
     *         FLGOP, FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
            IF (IERR.NE.0) GO TO 990
 15         CONTINUE
         DOIT = .FALSE.
         END IF
C                                       pointers
      NFL = 0
      IF (ALLPOL) THEN
         LPP1 = 1
         LPP2 = POLMAX
      ELSE
         LPP1 = POLNOW
         LPP2 = POLNOW
         END IF
      LR1 = EDCORI(PPTR+FLTIMS(1)-1)
      LR2 = EDCORI(PPTR+FLTIMS(2)) - 1
      LR2 = MIN (LR2, MAXREC)
      IF (DIMIF.GT.1) THEN
         LF0 = BIF
      ELSE
         LF0 = MAX (BIF, IFNOW)
         END IF
      IF (LR2.GE.LR1) THEN
         DO 50 LF = LF1,LF2
         DO 50 LPP = LPP1,LPP2
            IPTR = DIMPRM + (LPP-1) * DIMIF * DIMDAT + LF - LF0
            JPTR = IPTR + TYPE * DIMIF
            JJPT = IPTR + LTYPE * DIMIF
            IF (DOCOMP) JJPT2 = IPTR + LTYPE2 * DIMIF
            CCOLOR = (LF - 1) * POLMAX + LPP
            DO 30 LR = LR1,LR2
               LPTR = DPTR + (LR - 1) * DIMREC
               LA = EDCORI(LPTR)
               LT = EDCORI(LPTR+1)
               IF (EDOANT (ALLANT, IAN, LA)) THEN
                  IF ((FLTIMS(1).LE.LT) .AND. (FLTIMS(2).GE.LT)) THEN
                     IF ((SGN*EDCORE(LPTR+IPTR).LT.0.0) .OR.
     *                  ((FORCE) .AND. EDCORE(LPTR+IPTR).LT.0)) THEN
                        V = EDCORE(LPTR+JPTR)
C                                       flag it
                        IF (((V.NE.FBLANK) .OR. (BLNKOK)) .AND.
     *                     (LLFLUX(1).LE.V) .AND. (LLFLUX(2).GE.V)) THEN
                           NFL = NFL + 1
                           EDCORE(LPTR+IPTR) = ABS (EDCORE(LPTR+IPTR))
     *                        * SGN
C                                       change display
                           IF ((PLTAN(1).EQ.LA) .AND. ((PLTIF.EQ.LF)
     *                        .OR. (PLTIF.EQ.0)) .AND. ((PLTPOL.EQ.LPP)
     *                        .OR. (PLTPOL.EQ.0))) THEN
C                                       main
                              V = EDCORE(LPTR+JJPT)
                              WT = EDCORE(LPTR+IPTR)
                              AMP = EDCORE(LPTR+IPTR+DIMIF)
                              IF (DOE) CALL EDGSIG (LTYPE, WT, AMP, W1)
                              IF (DOE2) CALL EDGSIG (LTYPE2, WT, AMP,
     *                           W2)
                              IF (GRSEL(4).GT.0) THEN
                                 ITP = IROUND (SGN * GRSEL(4))
                                 CALL EDPLTP (XYPLOT, ITP, LT, TIME1,
     *                              TIME2, V, W1, APIXR, IERR)
                                 IF (IERR.GT.0) GO TO 980
                                 END IF
C                                       flagged
                              IF (GRSEL(5).GT.0) THEN
                                 ITP = - IROUND (SGN * GRSEL(5))
                                 CALL EDPLTP (XYPLOT, ITP, LT, TIME1,
     *                              TIME2, V, W1, APIXR, IERR)
                                 IF (IERR.GT.0) GO TO 980
                                 END IF
C                                       Comparison
                              IF (DOCOMP) V = EDCORE(LPTR+JJPT2)
                              IF ((GRSEL(4).GT.0) .AND. (DOCOMP)) THEN
                                 ITP = IROUND (SGN * GRSEL(4))
                                 CALL EDPLTP (EXPLOT, ITP, LT, TIME1,
     *                              TIME2, V, W2, APIXR2, IERR)
                                 IF (IERR.GT.0) GO TO 980
                                 END IF
C                                       flagged comparison
                              IF ((GRSEL(5).GT.0) .AND. (DOCOMP)) THEN
                                 ITP = - IROUND (SGN * GRSEL(5))
                                 CALL EDPLTP (EXPLOT, ITP, LT, TIME1,
     *                              TIME2, V, W2, APIXR2, IERR)
                                 IF (IERR.GT.0) GO TO 980
                                 END IF
C                                       2nd
                              IF ((GRSEL(7).GT.0) .AND. (DO2ND)) THEN
                                 V2 = EDCORE(LPTR+JJPT+4*DIMIF)
                                 ITP = IROUND (SGN * GRSEL(7))
                                 CALL EDPLTP (XYPLOT, ITP, LT, TIME1,
     *                              TIME2, V2, 0.0, APIXR, IERR)
                                 IF (IERR.GT.0) GO TO 980
                                 END IF
                              END IF
                           IF ((GRSEL(6).GT.0) .AND. ((PLTIF.EQ.LF) .OR.
     *                        (PLTIF.EQ.0)) .AND. ((PLTPOL.EQ.LPP) .OR.
     *                        (PLTPOL.EQ.0))) THEN
                              DO 20 I = 2,NUMPLT
                                 IF (PLTAN(I).EQ.LA) THEN
                                    ITP = IROUND (SGN * GRSEL(6))
                                    V = EDCORE(LPTR+JJPT)
                                    WT = EDCORE(LPTR+IPTR)
                                    AMP = EDCORE(LPTR+IPTR+DIMIF)
                                    IF (DOE) CALL EDGSIG (LTYPE, WT,
     *                                 AMP, W1)
                                    CALL EDPLTP (XYPLOT(1,I), ITP, LT,
     *                                 TIME1, TIME2, V, W1, APIXR(1,I),
     *                                 IERR)
                                    IF (IERR.GT.0) GO TO 980
                                    IF (GRSEL(5).GT.0) THEN
                                       ITP = - IROUND (SGN * GRSEL(5))
                                       CALL EDPLTP (XYPLOT(1,I), ITP,
     *                                    LT, TIME1, TIME2, V, W1,
     *                                    APIXR(1,I), IERR)
                                       IF (IERR.GT.0) GO TO 980
                                       END IF
                                    IF ((GRSEL(7).GT.0) .AND. (DO2ND))
     *                                 THEN
                                       V2 = EDCORE(LPTR+JJPT+4*DIMIF)
                                       ITP = IROUND (SGN * GRSEL(7))
                                       CALL EDPLTP (XYPLOT(1,I), ITP,
     *                                    LT, TIME1, TIME2, V2, 0.0,
     *                                    APIXR(1,I), IERR)
                                       IF (IERR.GT.0) GO TO 980
                                       END IF
                                    END IF
 20                              CONTINUE
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
 30            CONTINUE
 50         CONTINUE
         END IF
      IERR = 0
C                                       debug message
      IF (NFL.GT.0) FLAGED = .TRUE.
      IF (FLGMSG) THEN
         PRFLUX(1) = FLFLUX(1)
         PRFLUX(2) = FLFLUX(2)
         IF (FLFLUX(1).GT.-1.E12) PRFLUX(1) = FLFLUX(1) / DPLSCL(LTYPE)
         IF (FLFLUX(2).LT.1.E12) PRFLUX(2) = FLFLUX(2) / DPLSCL(LTYPE)
         IF (OP.NE.'UNFL') THEN
            WRITE (MSGTXT,1100) 'Flagged', NFL, FLTIMS, PRFLUX
            IF (IOP.LT.3) CALL MSGWRT (2)
         ELSE
            WRITE (MSGTXT,1100) 'Restored', NFL, FLTIMS, PRFLUX
            IF (IOP.LT.3) CALL MSGWRT (2)
            END IF
         END IF
      IERR = 0
      WASFLG = MAX (0, WASFLG) + NFL
      IF ((UVFLAG) .AND. (OP.NE.'UNFL')) THEN
         CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT,
     *      LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C
 980  MSGTXT = 'EDFLAG: ERROR UPDATING THE TV DISPLAY FOR FLAGGED DATA'
      CALL MSGWRT (6)
      GO TO 999
 990  MSGTXT = 'EDFLAG: ERROR UPDATING THE FLAG COMMAND TABLE'
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' points in TxF',2I7,' x',2(1PE10.2))
      END
      SUBROUTINE EDFLAI (TYPE, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX,
     *   FLPF, TVBUTT, IERR)
C-----------------------------------------------------------------------
C   interactive display of cursor position to set a flagging box
C   Inputs:
C      TYPE     I      Type of box: 0 vertical line, 1 vertical box,
C                      2 horiz box move top, 3 horiz box move bottom,
C                      4 area, 5 point, 6 quick point
C      CORN     I(4)   Keep box within these TV corners
C   In/out:
C      FIRST    I      > 0 -> first call of sequence, return 0
C                      0  -> TVCO already set
C                      < 0 -> no interaction, just clean up
C      TVXY     R(2)   TV cursor position - mode 6 uses on input
C      TVCO     I(4)   TV corners of current box
C   Output:
C      FLTIMS   I(2)   lower/upper time indices
C      FLFLUX   R(2)   lower/upper flux values of box
C      FLPF     I(2,2) Polarization,IF each end
C      TVBUTT   I      TV button that was pressed
C      IERR     I      Error code:0 okay, -1 no point
C-----------------------------------------------------------------------
      INTEGER   TYPE, CORN(4), FIRST, TVCO(4), FLTIMS(2), FLPF(2,2),
     *   TVBUTT, IERR
      REAL      TVXY(2), FLFLUX(2)
C
      INTEGER   TLC(4), NLINE, ILINE, IP, IX, I, ITT(4), ITRIM, LWIN(4),
     *   IT, IR, TCORN(4), FMODE, WTYPE
      REAL      X(3)
      LOGICAL   LFIRST
      CHARACTER STRING*16
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
C                                       get top left corner visible
      LWIN(1) = CORN(1) + 2
      LWIN(2) = CORN(2) + 2
      LWIN(3) = CORN(3) - 2
      LWIN(4) = CORN(4) - 2
      CALL TVDCRN (TVNAME, TCORN, IERR)
      IF (IERR.NE.0) GO TO 980
      TLC(1) = TCORN(1) + (CSIZE(1) + 1) / 2
      NLINE = 3
      ILINE = 1
      IF ((TYPE.EQ.2) .OR. (TYPE.EQ.3)) ILINE = 3
      TLC(ILINE+1) = TCORN(4) - CSIZE(2) - 1
      IF (ILINE+2.LE.4) TLC(ILINE+2) = TLC(ILINE+1) - CSIZE(2) -
     *   (CSIZE(2) + 1) / 2
      IF (ILINE+3.LE.4) TLC(ILINE+3) = TLC(ILINE+2) - CSIZE(2) -
     *   (CSIZE(2) + 1) / 2
C                                       where to put
      TCORN(1) = MAX (TCORN(1), LWIN(1))
      TCORN(2) = MAX (TCORN(2), LWIN(2))
      TCORN(3) = MIN (TCORN(3), LWIN(3))
      TCORN(4) = MIN (TCORN(4), LWIN(4))
C                                       initial call
      IF (FIRST.GT.0) THEN
         FIRST = 0
C                                       init graphics
         CALL TVDOPR (TVNAME, 'GRCL', GRSEL(3), IERR)
         IF (IERR.NE.0) GO TO 999
C                                       set TVCO
         CALL COPY (4, TCORN, TVCO)
         IF (TYPE.EQ.0) THEN
            TVCO(1) = (TCORN(1) + TCORN(3)) / 2
            TVCO(3) = TVCO(1)
         ELSE IF (TYPE.EQ.1) THEN
            TVCO(1) = (TCORN(1) + TCORN(3)) / 2 - 5
            TVCO(3) = TVCO(1) + 10
         ELSE IF (TYPE.EQ.2) THEN
            TVCO(4) = (TCORN(2) + TCORN(4)) / 2
         ELSE IF (TYPE.EQ.3) THEN
            TVCO(2) = (TCORN(2) + TCORN(4)) / 2
         ELSE IF (TYPE.EQ.4) THEN
            TVCO(1) = (TCORN(1) + TCORN(3)) / 2 - 5
            TVCO(2) = (TCORN(2) + TCORN(4)) / 2 - 5
            TVCO(3) = (TCORN(1) + TCORN(3)) / 2 + 5
            TVCO(4) = (TCORN(2) + TCORN(4)) / 2 + 5
         ELSE IF ((TYPE.EQ.5) .OR. (TYPE.EQ.6)) THEN
            TVCO(1) = (TCORN(1) + TCORN(3)) / 2
            TVCO(2) = (TCORN(2) + TCORN(4)) / 2
            TVCO(3) = 5
            TVCO(4) = 5
         ELSE
            IERR = 2
            GO TO 980
            END IF
         CALL EDDBOX (TVNAME, TYPE, 1, GRSEL(3), TVCO, IERR)
         IF (IERR.NE.0) GO TO 999
         TVXY(1) = TVCO(1)
         TVXY(2) = TVCO(2)
         LFIRST = .TRUE.
         END IF
C                                       now interact
      IF (FIRST.EQ.0) THEN
         IP = 1
         IF (TYPE.NE.6) THEN
            TVXY(1) = TVCO(1)
            TVXY(2) = TVCO(2)
            LFIRST = .TRUE.
            END IF
         IF ((TYPE.EQ.0) .OR. (TYPE.EQ.1)) THEN
            TVXY(2) = (TCORN(2) + TCORN(4)) / 2
         ELSE IF ((TYPE.EQ.2) .OR. (TYPE.EQ.3)) THEN
            TVXY(1) = (TCORN(1) + TCORN(3)) / 2
            END IF
         IF (TYPE.EQ.2) TVXY(2) = TVCO(4)
C                                       read cursor until something
 100     CALL TVDINT (TVNAME, LWIN, LFIRST, TVXY, TVBUTT, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       off old box
         CALL EDDBOX (TVNAME, TYPE, 3, GRSEL(3), TVCO, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       new corner
         IF (IP.EQ.1) THEN
            IF (TYPE.EQ.0) THEN
               TVCO(1) = TVXY(1) + 0.5
               TVCO(3) = TVCO(1)
               TVXY(1) = TVCO(1)
            ELSE IF (TYPE.EQ.1) THEN
               TVCO(1) = TVXY(1)
               TVXY(1) = TVCO(1)
            ELSE IF (TYPE.EQ.2) THEN
               TVCO(4) = TVXY(2) + 0.8
               TVXY(2) = TVCO(4)
            ELSE IF (TYPE.EQ.3) THEN
               TVCO(2) = TVXY(2)
               TVXY(2) = TVCO(2)
            ELSE IF (TYPE.EQ.4) THEN
               TVCO(1) = TVXY(1)
               TVCO(2) = TVXY(2)
               TVXY(1) = TVCO(1)
               TVXY(2) = TVCO(2)
            ELSE IF ((TYPE.EQ.5) .OR. (TYPE.EQ.6)) THEN
               TVCO(1) = TVXY(1) + 0.5
               TVCO(2) = TVXY(2) + 0.5
               END IF
         ELSE
            TVCO(3) = TVXY(1) + 0.8
            TVXY(1) = TVCO(3)
            IF (TYPE.EQ.4) THEN
               TVCO(4) = TVXY(2) + 0.8
               TVXY(2) = TVCO(4)
               END IF
            END IF
C                                       on new box
         CALL EDDBOX (TVNAME, TYPE, 1, GRSEL(3), TVCO, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       label
         X(1) = (TIME2 - TIME1) * (TVXY(1) - XYPLOT(1,1) - LEDG)
     *      / REAL (XYPLOT(3,1) - XYPLOT(1,1) - 2*LEDG) + TIME1
         IF ((TYPE.NE.2) .AND. (TYPE.NE.3)) THEN
            IF ((TYPE.EQ.5) .OR. (TYPE.EQ.6)) THEN
               FMODE = 0
               WTYPE = 1
            ELSE
               FMODE = 3
               WTYPE = 0
               END IF
            IF ((TYPE.EQ.1) .OR. (TYPE.EQ.4)) THEN
               IF (TVCO(1).LT.TVCO(3)) THEN
                  FMODE = 3 + IP
               ELSE
                  FMODE = 6 - IP
                  END IF
               END IF
            CALL EDFIND (FMODE, WTYPE, PLTAN(1), LTYPE, TVXY, X, FLPF,
     *         IT, IR, IERR)
            IF (IERR.GT.0) GO TO 999
            END IF
C                                       use cursor vertical some modes
         IF ((TYPE.GE.2) .AND. (TYPE.LE.4)) X(2) =
     *      (APIXR(2,1)-APIXR(1,1)) * (TVXY(2)-XYPLOT(2,1)-LEDG) /
     *      REAL (XYPLOT(4,1) - XYPLOT(2,1) - 2*LEDG) + APIXR(1,1)
         X(2) = X(2) / DPLSCL(LTYPE)
         DO 110 I = ILINE,NLINE
            IF (I.EQ.1) THEN
               IX = X(1) + 0.5
               IX = EDCORI(IX+SUPTR-1)
               IF ((IX.GE.1) .AND. (IX.LE.NSRC)) THEN
                  STRING = SLIST(IX)
               ELSE
                  STRING = ' '
                  END IF
            ELSE IF (I.EQ.2) THEN
               IX = X(1) + 0.5
               X(1) = TIMES(EDIPTR+IX)
               CALL TODHMS (X(1), ITT)
               WRITE (STRING,1100) ITT
            ELSE IF (LTYPE.EQ.1) THEN
               IF (DDTYPE.EQ.'TY') THEN
                  WRITE (STRING,1111) X(2)
                  IF (X(2).GT.10.0) WRITE (STRING,1116) X(2)
               ELSE IF (DDTYPE.EQ.'SY') THEN
                  WRITE (STRING,1131) X(2)
               ELSE IF (MULTIB) THEN
                  IF ((ABS(X(2)).GT.0.005) .AND. (X(2).GT.-99.9) .AND.
     *               (X(2).LT.999.9)) THEN
                     WRITE (STRING,1103) 'MBDLY', X(2)
                  ELSE
                     WRITE (STRING,1104) 'MBDLY', X(2)
                     END IF
               ELSE
                  WRITE (STRING,1101) X(2)
                  IF (MULTIB) STRING(1:4) = 'NBDL'
                  END IF
            ELSE IF (LTYPE.EQ.2) THEN
               IF (DDTYPE.EQ.'TY') THEN
                  WRITE (STRING,1112) X(2)
                  IF (X(2).GT.10.0) WRITE (STRING,1117) X(2)
               ELSE IF (DDTYPE.EQ.'SY') THEN
                  WRITE (STRING,1132) X(2)
               ELSE
                  WRITE (STRING,1102) X(2)
                  END IF
            ELSE IF (LTYPE.EQ.3) THEN
               IF (DDTYPE.EQ.'TY') THEN
                  WRITE (STRING,1105) X(2)
               ELSE IF (DDTYPE.EQ.'SY') THEN
                  WRITE (STRING,1133) X(2)
               ELSE IF (DDTYPE.EQ.'UV') THEN
                  WRITE (STRING,1123) X(2)
               ELSE
                  IF ((ABS(X(2)).GT.0.005) .AND. (X(2).GT.-99.9) .AND.
     *               (X(2).LT.999.9)) THEN
                     WRITE (STRING,1103) 'DELAY', X(2)
                  ELSE
                     WRITE (STRING,1104) 'DELAY', X(2)
                     END IF
                  END IF
            ELSE IF (LTYPE.EQ.4) THEN
               IF (DDTYPE.EQ.'TY') THEN
                  WRITE (STRING,1105) X(2)
               ELSE IF (DDTYPE.EQ.'SY') THEN
                  WRITE (STRING,1134) X(2)
               ELSE IF (DDTYPE.EQ.'UV') THEN
                  WRITE (STRING,1124) X(2)
               ELSE
                  IF ((ABS(X(2)).GT.0.005) .AND. (X(2).GT.-99.9) .AND.
     *               (X(2).LT.999.9)) THEN
                     WRITE (STRING,1103) 'RATE ', X(2)
                  ELSE
                     WRITE (STRING,1104) 'RATE ', X(2)
                     END IF
                  END IF
            ELSE IF ((LTYPE.EQ.5) .AND. (DDTYPE.EQ.'SY')) THEN
               WRITE (STRING,1135) X(2)
            ELSE IF ((LTYPE.EQ.6) .AND. (DDTYPE.EQ.'SY')) THEN
               WRITE (STRING,1136) X(2)
            ELSE IF ((LTYPE.EQ.7) .AND. (DDTYPE.EQ.'SY')) THEN
               WRITE (STRING,1137) X(2)
            ELSE IF ((LTYPE.EQ.8) .AND. (DDTYPE.EQ.'SY')) THEN
               WRITE (STRING,1138) X(2)
            ELSE
               WRITE (STRING,1105) X(2)
               END IF
            IX = ITRIM (STRING)
            IF (I.EQ.1) IX = 16
            CALL TVDCHR (TVNAME, TLC(1), TLC(1+I), 0, 0, GRSEL(3),
     *         STRING(:IX), IERR)
            IF (IERR.NE.0) GO TO 980
 110        CONTINUE
         X(2) = X(2) * DPLSCL(LTYPE)
C                                       switch
         IF ((TVBUTT.EQ.1) .AND. ((TYPE.EQ.1) .OR. (TYPE.EQ.4))) THEN
            TVBUTT = 0
            IP = 3 - IP
            LFIRST = .TRUE.
            IF (IP.EQ.1) THEN
               TVXY(1) = TVCO(1)
               TVXY(2) = TVCO(2)
            ELSE
               TVXY(1) = TVCO(3)
               TVXY(2) = TVCO(4)
               END IF
            IF (TYPE.EQ.1) TVXY(2) = (TCORN(2) + TCORN(4)) / 2
            END IF
C                                       keep going
         IF ((TVBUTT.LE.0) .AND. (TYPE.NE.6)) GO TO 100
C                                       check order
         IF ((TYPE.NE.5) .AND. (TYPE.NE.6)) THEN
            IF (TVCO(1).GT.TVCO(3)) THEN
               IX = TVCO(1)
               TVCO(1) = TVCO(3)
               TVCO(3) = IX
               END IF
            IF (TVCO(2).GT.TVCO(4)) THEN
               IX = TVCO(2)
               TVCO(2) = TVCO(4)
               TVCO(4) = IX
               END IF
            END IF
C                                       return stuff
         IF (TYPE.EQ.0) THEN
            TVXY(1) = TVCO(1)
            CALL EDFIND (1, 0, PLTAN(1), LTYPE, TVXY, X, FLPF(1,1), IT,
     *         IR, IERR)
            IF (IERR.GT.0) GO TO 999
            IF (IERR.EQ.0) THEN
               FLTIMS(1) = IT
               CALL EDFIND (2, 0, PLTAN(1), LTYPE, TVXY, X, FLPF(1,2),
     *            IT, IR, IERR)
               IF (IERR.GT.0) GO TO 999
               IERR = 0
               FLTIMS(2) = IT
            ELSE
               CALL EDFIND (3, 0, PLTAN(1), LTYPE, TVXY, X, FLPF, IT,
     *            IR, IERR)
               IF (IERR.GT.0) GO TO 999
               IERR = 0
               FLTIMS(1) = IT
               FLTIMS(2) = IT
               FLPF(1,2) = FLPF(1,1)
               FLPF(2,2) = FLPF(2,1)
               END IF
         ELSE IF ((TYPE.EQ.5) .OR. (TYPE.EQ.6)) THEN
            TVXY(1) = TVCO(1)
            TVXY(2) = TVCO(2)
            CALL EDFIND (0, 1, PLTAN(1), LTYPE, TVXY, X, FLPF, IT, IR,
     *         IERR)
            IF (IERR.GT.0) GO TO 999
            FLTIMS(1) = IT
            FLTIMS(2) = IT
            FLFLUX(1) = X(2)
            FLFLUX(2) = X(2)
            FLPF(1,2) = FLPF(1,1)
            FLPF(2,2) = FLPF(2,1)
         ELSE
            TVXY(1) = TVCO(1)
            CALL EDFIND (4, 0, PLTAN(1), LTYPE, TVXY, X, FLPF(1,1), IT,
     *         IR, IERR)
            IF (IERR.GT.0) GO TO 999
            FLTIMS(1) = IT
            TVXY(1) = TVCO(3)
            CALL EDFIND (5, 0, PLTAN(1), LTYPE, TVXY, X, FLPF(1,2), IT,
     *         IR, IERR)
            IF (IERR.GT.0) GO TO 999
            FLTIMS(2) = IT
            END IF
         IF ((TYPE.NE.5) .AND. (TYPE.NE.6)) THEN
            FLFLUX(1) = (APIXR(2,1) - APIXR(1,1)) * REAL (TVCO(2) -
     *         XYPLOT(2,1) - LEDG) / REAL (XYPLOT(4,1) - XYPLOT(2,1) -
     *         2*LEDG) + APIXR(1,1)
            FLFLUX(2) = (APIXR(2,1) - APIXR(1,1)) * REAL (TVCO(4) -
     *         XYPLOT(2,1) - LEDG) / REAL (XYPLOT(4,1) - XYPLOT(2,1) -
     *         2*LEDG) + APIXR(1,1)
            END IF
         END IF
C                                       close down - init graphics
      IF (FIRST.LT.0) THEN
         CALL TVDOPR (TVNAME, 'GRCL', GRSEL(3), IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      GO TO 999
C
 980  MSGTXT = 'EDFLAI: ERROR SETTING FLAG WINDOW'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('TIME',I3,'/',2(I2.2,':'),I2.2)
 1101 FORMAT ('AMPLI',F7.3)
 1102 FORMAT ('PHASE',F7.1)
 1103 FORMAT (A,F9.4)
 1104 FORMAT (A,1PE11.3)
 1105 FORMAT ('?????',1PE11.3)
 1111 FORMAT ('T SYS',F8.4)
 1112 FORMAT ('T DIFF',F8.4)
 1116 FORMAT ('T SYS',F8.2)
 1117 FORMAT ('T DIFF',F7.2)
 1123 FORMAT ('D AMPLI',F7.3)
 1124 FORMAT ('D PHASE',F7.1)
 1131 FORMAT ('P DIF',F8.2)
 1132 FORMAT ('P SUM',F8.2)
 1133 FORMAT ('P GAIN',F8.3)
 1134 FORMAT ('P TSYS',F8.2)
 1135 FORMAT ('D PDIF',F8.3)
 1136 FORMAT ('D PSUM',F8.3)
 1137 FORMAT ('D PGAIN',F8.3)
 1138 FORMAT ('D PSYS',F8.3)
      END
      SUBROUTINE EDFLAR (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of an area: time range and flux range
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), NFL,
     *   LLTIMS(2), FOP, IFL, PRTIMS(2), POLOLD, IFOLD, IP1, IP2, IF1,
     *   IF2, FLPF(2,2)
      REAL      TVXY(2), FLFLUX(2), PRFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      POLOLD = POLNOW
      IFOLD = IFNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (IFOLD.EQ.0) THEN
         IF1 = BIF
         IF2 = EIF
      ELSE
         IF1 = IFNOW
         IF2 = IFNOW
         END IF
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         MSGTXT = 'Hit button A to set other corner of the area'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to flag area and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to flag area and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
C                                       read cursor until button
      POLNOW = POLOLD
      IFNOW = IFOLD
      CALL EDFLAI (4, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         FOP = 5
         IFL = 0
         PRTIMS(1) = FLTIMS(1)
         PRTIMS(2) = FLTIMS(2)
         PLTIF = IFOLD
         PLTPOL = POLOLD
C                                       Loop for each point
C                                       individually
         DO 40 IFNOW = IF1,IF2
            DO 30 POLNOW = IP1,IP2
               FLTIMS(1) = PRTIMS(1)
 20            CALL EDGTAP (PLTAN(1), LTYPE, FLTIMS, FLFLUX, IERR)
               IF (IERR.GT.0) GO TO 999
               IF (IERR.EQ.0) THEN
                  LLTIMS(1) = FLTIMS(1)
                  LLTIMS(2) = FLTIMS(1)
                  CALL EDFLAG ('FORC', FOP, PLTAN(1), LTYPE, LLTIMS,
     *               FLFLUX, NFL, IERR)
                  IF (IERR.GT.0) GO TO 980
                  IFL = IFL + NFL
                  IF (NFL.GT.0) FOP = -5
                  FLTIMS(1) = FLTIMS(1) + 1
                  IF (FLTIMS(1).LE.FLTIMS(2)) GO TO 20
                  END IF
 30            CONTINUE
 40         CONTINUE
         PRFLUX(1) = FLFLUX(1) / DPLSCL(LTYPE)
         PRFLUX(2) = FLFLUX(2) / DPLSCL(LTYPE)
         WRITE (MSGTXT,1100) 'Flagged', IFL, PRTIMS, PRFLUX
         CALL MSGWRT (2)
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (4, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDFLAR: ERROR DOING AREA FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' points in TxF',2I7,' x',2(1PE10.2))
      END
      SUBROUTINE EDFLFA (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of high fluxes
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), NFL, IFL,
     *   FLPF(2,2), FOP, POLOLD, IFOLD, LLTIMS(2), PRTIMS(2), IP1, IP2,
     *   IF1, IF2
      REAL      TVXY(2), FLFLUX(2), PRFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      POLOLD = POLNOW
      IFOLD = IFNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (IFOLD.EQ.0) THEN
         IF1 = BIF
         IF2 = EIF
      ELSE
         IF1 = IFNOW
         IF2 = IFNOW
         END IF
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         MSGTXT = 'Hit buttons A or B to flag high fluxes and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to flag high fluxes and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
C                                       read cursor until button
      CALL EDFLAI (3, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         IF (ALLTIM) THEN
            FLTIMS(1) = 1
            FLTIMS(2) = TIMEM - 1
         ELSE
            FLTIMS(1) = TIME1
            FLTIMS(2) = TIME2
            END IF
         FLFLUX(2) = 1.E20
         FOP = 4
         IFL = 0
         PRTIMS(1) = FLTIMS(1)
         PRTIMS(2) = FLTIMS(2)
         PLTIF = IFOLD
         PLTPOL = POLOLD
C                                       Loop for each point
C                                       individually
         DO 40 IFNOW = IF1,IF2
            DO 30 POLNOW = IP1,IP2
               FLTIMS(1) = PRTIMS(1)
 20            CALL EDGTAP (PLTAN(1), LTYPE, FLTIMS, FLFLUX, IERR)
               IF (IERR.GT.0) GO TO 999
               IF (IERR.EQ.0) THEN
                  LLTIMS(1) = FLTIMS(1)
                  LLTIMS(2) = FLTIMS(1)
                  CALL EDFLAG ('FORC', FOP, PLTAN(1), LTYPE, LLTIMS,
     *               FLFLUX, NFL, IERR)
                  IF (IERR.GT.0) GO TO 980
                  IFL = IFL + NFL
                  IF (NFL.GT.0) FOP = -4
                  FLTIMS(1) = FLTIMS(1) + 1
                  IF (FLTIMS(1).LE.FLTIMS(2)) GO TO 20
                  END IF
 30            CONTINUE
 40         CONTINUE
         PRFLUX(1) = FLFLUX(1) / DPLSCL(LTYPE)
         WRITE (MSGTXT,1100) 'Flagged', IFL, PRTIMS, PRFLUX(1)
         CALL MSGWRT (2)
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (3, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDFLFA: ERROR DOING FLUX ABOVE FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' points in T',2I7,'  F >',1PE11.3)
      END
      SUBROUTINE EDFLFB (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of low fluxes
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), NFL, IFL,
     *   FLPF(2,2), FOP, POLOLD, IFOLD, LLTIMS(2), PRTIMS(2), IP1, IP2,
     *   IF1, IF2
      REAL     TVXY(2),  FLFLUX(2), PRFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      POLOLD = POLNOW
      IFOLD = IFNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (IFOLD.EQ.0) THEN
         IF1 = BIF
         IF2 = EIF
      ELSE
         IF1 = IFNOW
         IF2 = IFNOW
         END IF
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         MSGTXT = 'Hit buttons A or B to flag time and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to flag time and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
C                                       read cursor until button
      CALL EDFLAI (2, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         IF (ALLTIM) THEN
            FLTIMS(1) = 1
            FLTIMS(2) = TIMEM - 1
         ELSE
            FLTIMS(1) = TIME1
            FLTIMS(2) = TIME2
            END IF
         FLFLUX(1) = -1.E20
         FOP = 3
         IFL = 0
         PRTIMS(1) = FLTIMS(1)
         PRTIMS(2) = FLTIMS(2)
         PLTIF = IFOLD
         PLTPOL = POLOLD
C                                       Loop for each point
C                                       individually
         DO 40 IFNOW = IF1,IF2
            DO 30 POLNOW = IP1,IP2
               FLTIMS(1) = PRTIMS(1)
 20            CALL EDGTAP (PLTAN(1), LTYPE, FLTIMS, FLFLUX, IERR)
               IF (IERR.GT.0) GO TO 999
               IF (IERR.EQ.0) THEN
                  LLTIMS(1) = FLTIMS(1)
                  LLTIMS(2) = FLTIMS(1)
                  CALL EDFLAG ('FORC', FOP, PLTAN(1), LTYPE, LLTIMS,
     *               FLFLUX, NFL, IERR)
                  IF (IERR.GT.0) GO TO 980
                  IFL = IFL + NFL
                  IF (NFL.GT.0) FOP = -3
                  FLTIMS(1) = FLTIMS(1) + 1
                  IF (FLTIMS(1).LE.FLTIMS(2)) GO TO 20
                  END IF
 30            CONTINUE
 40         CONTINUE
         PRFLUX(2) = FLFLUX(2) / DPLSCL(LTYPE)
         WRITE (MSGTXT,1100) 'Flagged', IFL, PRTIMS, PRFLUX(2)
         CALL MSGWRT (2)
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (2, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDFLFB: ERROR DOING FLUX BERLOW FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' points in T',2I7,'   F <',1PE11.3)
      END
      SUBROUTINE EDFLPT (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of points (uses Y and X position)
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), NFL, NM,
     *   FLPF(2,2), POLOLD, IFOLD, IT, IR
      REAL      TVXY(2), FLFLUX(2), X(3)
      LOGICAL   DOMSG
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      NM = 0
      POLOLD = POLNOW
      IFOLD = IFNOW
      DOMSG = .TRUE.
C                                       instructions
 10   IF ((TVBUTT.GT.0) .AND. (NM.EQ.0)) THEN
         MSGTXT = 'Hit buttons A or B to flag sample and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to flag sample and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
      NM = MOD (NM+1, 10)
C                                       read cursor until button
      POLNOW = POLOLD
      IFNOW = IFOLD
      CALL EDFLAI (5, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
      PLTIF = IFOLD
      PLTPOL = POLOLD
C                                       flag something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7)) THEN
         IF (IERR.EQ.0) THEN
 20         POLNOW = FLPF(1,1)
            IFNOW = FLPF(2,1)
            CALL EDFLAG ('FLAG', 6, PLTAN(1), LTYPE, FLTIMS, FLFLUX,
     *         NFL, IERR)
            IF (IERR.GT.0) GO TO 980
            DOMSG = .TRUE.
C                                       crowded may have more than one
            IF ((IFOLD.LE.0) .OR. (POLOLD.LE.0)) THEN
               POLNOW = POLOLD
               IFNOW = IFOLD
               PLTIF = IFOLD
               PLTPOL = POLOLD
               CALL EDFIND (0, 1, PLTAN(1), LTYPE, TVXY, X, FLPF, IT,
     *            IR, IERR)
               IF (IERR.GT.0) GO TO 999
               FLTIMS(1) = IT
               FLTIMS(2) = IT
               FLFLUX(1) = X(2)
               FLFLUX(2) = X(2)
               FLPF(1,2) = FLPF(1,1)
               FLPF(2,2) = FLPF(2,1)
               IF (IERR.EQ.0) GO TO 20
               END IF
         ELSE
            MSGTXT = 'No unflagged sample in the vicinity'
            IF (DOMSG) CALL MSGWRT (1)
            DOMSG = .FALSE.
            END IF
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (5, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDFLPT: ERROR DOING POINT FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE EDFLQU (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of points in quick mode
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), NFL, IT, IR,
     *   FLPF(2,2), POLOLD, IFOLD, NM
      REAL      TVXY(2), FLFLUX(2), X(3)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      POLOLD = POLNOW
      IFOLD = IFNOW
      NM = 0
C                                       instructions
 10   IF ((TVBUTT.GT.0) .OR. (NM.EQ.0)) THEN
         MSGTXT = 'Click left mouse button to flag sample and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons A, B, or C to flag sample and return'
     *      // ' to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
      NM = MAX (NM, 1)
C                                       read cursor until button
      POLNOW = POLOLD
      IFNOW = IFOLD
      PLTIF = IFOLD
      PLTPOL = POLOLD
      CALL EDFLAI (6, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      IF ((TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
 20      POLNOW = FLPF(1,1)
         IFNOW = FLPF(2,1)
         CALL EDFLAG ('FLAG', 6, PLTAN(1), LTYPE, FLTIMS, FLFLUX, NFL,
     *      IERR)
         IF (IERR.GT.0) GO TO 980
         IF (NM.NE.0) NM = MOD (NM+1, 20)
C                                       crowded may have more than one
         IF ((IFOLD.LE.0) .OR. (POLOLD.LE.0)) THEN
            POLNOW = POLOLD
            IFNOW = IFOLD
            PLTIF = IFOLD
            PLTPOL = POLOLD
            CALL EDFIND (0, 1, PLTAN(1), LTYPE, TVXY, X, FLPF, IT, IR,
     *         IERR)
            IF (IERR.GT.0) GO TO 999
            FLTIMS(1) = IT
            FLTIMS(2) = IT
            FLFLUX(1) = X(2)
            FLFLUX(2) = X(2)
            FLPF(1,2) = FLPF(1,1)
            FLPF(2,2) = FLPF(2,1)
            IF (IERR.EQ.0) GO TO 20
            END IF
         END IF
C                                       loop
      IF (TVBUTT.LE.0) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (6, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDFLQU: ERROR DOING FAST POINT FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE EDFLTI (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of a single time
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), NFL, POLOLD,
     *   IFOLD, IP1, IP2, IF1, IF2, FLPF(2,2), FOP, NM, I
      REAL      TVXY(2), FLFLUX(2)
      INCLUDE 'EDIUTIL.INC'
      LOGICAL   DONEIF(MAXIF,2)
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      NM = 0
      FIRST = 1
      POLOLD = POLNOW
      IFOLD = IFNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLOLD
         IP2 = POLOLD
         END IF
      IF (IFOLD.EQ.0) THEN
         IF1 = BIF
         IF2 = EIF
      ELSE
         IF1 = IFOLD
         IF2 = IFOLD
         END IF
C                                       instructions
 10   IF ((TVBUTT.GT.0) .AND. (NM.EQ.0)) THEN
         MSGTXT = 'Hit buttons A or B to flag time and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to flag time and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
      NM = MOD (NM+1, 7)
C                                       read cursor until button
      POLNOW = POLOLD
      IFNOW = IFOLD
      CALL EDFLAI (0, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      FLFLUX(1) = -1.E20
      FLFLUX(2) = 1.E20
      FOP = 1
      PLTIF = IFOLD
      PLTPOL = POLOLD
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         IF (ALLIF.NE.0) THEN
            I = EIF - BIF + 1
            CALL LFILL (I, .FALSE., DONEIF(BIF,1))
            CALL LFILL (I, .FALSE., DONEIF(BIF,2))
         ELSE
            I = EIF - BIF + 1
            CALL LFILL (I, .TRUE., DONEIF(BIF,1))
            CALL LFILL (I, .TRUE., DONEIF(BIF,2))
            I = FGIFRS(2) - FGIFRS(1) + 1
            CALL LFILL (I, .FALSE., DONEIF(FGIFRS(1),1))
            CALL LFILL (I, .FALSE., DONEIF(FGIFRS(1),2))
            END IF
         DO 40 IFNOW = IF1,IF2
            DO 30 POLNOW = IP1,IP2
               IF (.NOT.DONEIF(IFNOW,POLNOW)) THEN
                  CALL EDGTPT (PLTAN(1), LTYPE, FLTIMS, FLFLUX, IERR)
                  IF (IERR.GT.0) GO TO 999
                  IF (IERR.EQ.0) THEN
                     IF (ALLIF.EQ.1) THEN
                        I = EIF - BIF + 1
                        CALL LFILL (I, .TRUE., DONEIF(BIF,POLNOW))
                        IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                     DONEIF(BIF,3-POLNOW))
                     ELSE IF (ALLIF.EQ.0) THEN
                        I = FGIFRS(2) - FGIFRS(1) + 1
                        CALL LFILL (I, .TRUE.,
     *                     DONEIF(FGIFRS(1),POLNOW))
                        IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                     DONEIF(FGIFRS(1),3-POLNOW))
                     ELSE
                        DONEIF(IFNOW,POLNOW) = .TRUE.
                        IF (ALLPOL) DONEIF(IFNOW,3-POLNOW) = .TRUE.
                        END IF
                     CALL EDFLAG ('FLAG', FOP, PLTAN(1), LTYPE, FLTIMS,
     *                  FLFLUX, NFL, IERR)
                     IF (IERR.GT.0) GO TO 980
                     END IF
                  END IF
 30            CONTINUE
 40         CONTINUE
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (0, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDFLTI: ERROR DOING SINGLE-TIME FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE EDFLTR (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of a time-range
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), NFL, POLOLD,
     *   IFOLD, IP1, IP2, IF1, IF2, FLPF(2,2), FOP, PRTIMS(2), NM, I
      REAL      TVXY(2), FLFLUX(2)
      INCLUDE 'EDIUTIL.INC'
      LOGICAL   DONEIF(MAXIF,2)
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      POLOLD = POLNOW
      IFOLD = IFNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (IFOLD.EQ.0) THEN
         IF1 = BIF
         IF2 = EIF
      ELSE
         IF1 = IFNOW
         IF2 = IFNOW
         END IF
      NM = 0
C                                       instructions
 10   IF ((TVBUTT.GT.0) .AND. (NM.EQ.0)) THEN
         MSGTXT = 'Hit button A to set the other side of the time range'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to flag time range and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to flag time range and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
      NM = MOD (NM+1, 4)
C                                       read cursor until button
      POLNOW = POLOLD
      IFNOW = IFOLD
      CALL EDFLAI (1, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      FLFLUX(1) = -1.E20
      FLFLUX(2) = 1.E20
      FOP = 2
      PLTIF = IFOLD
      PLTPOL = POLOLD
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         IF (ALLIF.NE.0) THEN
            I = EIF - BIF + 1
            CALL LFILL (I, .FALSE., DONEIF(BIF,1))
            CALL LFILL (I, .FALSE., DONEIF(BIF,2))
         ELSE
            I = EIF - BIF + 1
            CALL LFILL (I, .TRUE., DONEIF(BIF,1))
            CALL LFILL (I, .TRUE., DONEIF(BIF,2))
            I = FGIFRS(2) - FGIFRS(1) + 1
            CALL LFILL (I, .FALSE., DONEIF(FGIFRS(1),1))
            CALL LFILL (I, .FALSE., DONEIF(FGIFRS(1),2))
            END IF
         DO 40 IFNOW = IF1,IF2
            DO 30 POLNOW = IP1,IP2
               IF (.NOT.DONEIF(IFNOW,POLNOW)) THEN
                  PRTIMS(1) = FLTIMS(1)
                  PRTIMS(2) = FLTIMS(2)
                  CALL EDGTPT (PLTAN(1), LTYPE, PRTIMS, FLFLUX, IERR)
                  IF (IERR.GT.0) GO TO 999
                  IF (IERR.EQ.0) THEN
                     IF (ALLIF.EQ.1) THEN
                        I = EIF - BIF + 1
                        CALL LFILL (I, .TRUE., DONEIF(BIF,POLNOW))
                        IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                     DONEIF(BIF,3-POLNOW))
                     ELSE IF (ALLIF.EQ.0) THEN
                        I = FGIFRS(2) - FGIFRS(1) + 1
                        CALL LFILL (I, .TRUE.,
     *                     DONEIF(FGIFRS(1),POLNOW))
                        IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                     DONEIF(FGIFRS(1),3-POLNOW))
                     ELSE
                        DONEIF(IFNOW,POLNOW) = .TRUE.
                        IF (ALLPOL) DONEIF(IFNOW,3-POLNOW) = .TRUE.
                        END IF
                     CALL EDFLAG ('FLAG', FOP, PLTAN(1), LTYPE, FLTIMS,
     *                  FLFLUX, NFL, IERR)
                     IF (IERR.GT.0) GO TO 980
                     END IF
                  END IF
 30            CONTINUE
 40         CONTINUE
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (1, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDFLTR: ERROR DOING TIME-RANGE FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE EDFRAM (IERR)
C-----------------------------------------------------------------------
C   interactive setting of display frame
C   Outputs:
C      IERR     I   Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT
      LOGICAL   FIRST, LOWER
      REAL      TVXY(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVXY(2) = (CORN(4) + CORN(2)) / 2
      LOWER = .TRUE.
      TVBUTT = 1
      CALL EDTWIN ('ON', 0, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         IF (LOWER) THEN
            TVBUTT = (TIME1 - 1.0) / (TIMEM - 1.0) * (CORN(3) - CORN(1)
     *         - 2*LEDG) + 0.5 + CORN(1) + LEDG
            MSGTXT = 'Setting lower limit with cursor X'
            CALL MSGWRT (1)
            MSGTXT = 'Hit buttons A or B to do upper limit'
         ELSE
            TVBUTT = (TIME2 - 1.0) / (TIMEM - 1.0) * (CORN(3) - CORN(1)
     *         - 2*LEDG) + 0.5 + CORN(1) + LEDG
            MSGTXT = 'Setting upper limit with cursor X'
            CALL MSGWRT (1)
            MSGTXT = 'Hit buttons A or B to do lower limit'
            END IF
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons C or D to exit'
         CALL MSGWRT (1)
         FIRST = .TRUE.
         TVXY(1) = TVBUTT
         END IF
C                                       read cursor until something
      CALL TVDINT (TVNAME, CORN, FIRST, TVXY, TVBUTT, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       reset lower
      IF (LOWER) THEN
         CALL EDTWIN ('OFF', 1, IERR)
         IF (IERR.NE.0) GO TO 980
         TIME1 = (TVXY(1) - LEDG - CORN(1)) * (TIMEM - 1.0) /
     *      (CORN(3) - CORN(1) - 2*LEDG) + 1.5
         TIME1 = MAX (1, MIN (TIMEM-1, TIME1))
         CALL EDTWIN ('ON', 1, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       right line
      ELSE
         CALL EDTWIN ('OFF', 2, IERR)
         IF (IERR.NE.0) GO TO 980
         TIME2 = (TVXY(1) - LEDG - CORN(1)) * (TIMEM - 1.0) /
     *      (CORN(3) - CORN(1) - 2*LEDG) + 1.5
         TIME2 = MAX (1, MIN (TIMEM-1, TIME2))
         CALL EDTWIN ('ON', 2, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      IF (TVBUTT.GT.0) LOWER = .NOT.LOWER
      IF (TVBUTT.LT.4) GO TO 10
      IF (TIME2.LT.TIME1) THEN
         TVBUTT = TIME1
         TIME1 = TIME2
         TIME2 = TVBUTT
         END IF
      CALL EDTWIN ('OFF', 0, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 999
C
 980  MSGTXT = 'EDFRAM: ERROR SETTING FRAME INTERACTIVELY'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE EDG1DA (FLTIMS, IERR)
C-----------------------------------------------------------------------
C   read in a time-range of data, replacing the selected in-core data
C   All data types are replaced.
C   Inputs:
C      IGS      I(4)   Graphics plane of data, extra data, flagged, top
C      FLTIMS   I(2)   Time array indices of flag window
C   Output:
C      IERR     I      Error code: 0 -> okay
C-----------------------------------------------------------------------
      INTEGER   FLTIMS(2), IERR
C
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
C                                       find the data
      IF ((DDTYPE.EQ.'SN') .OR. (DDTYPE.EQ.'CL')) THEN
         CALL EDG1SN (PLTAN(1), FLTIMS, IERR)
      ELSE IF (DDTYPE.EQ.'TY') THEN
         CALL EDG1TY (PLTAN(1), FLTIMS, IERR)
      ELSE IF (DDTYPE.EQ.'SY') THEN
         CALL EDG1SY (PLTAN(1), FLTIMS, IERR)
      ELSE IF (DDTYPE.EQ.'UV') THEN
         IERR = 1
      ELSE
         IERR = 2
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDG1SN (IAN, FLTIMS, IERR)
C-----------------------------------------------------------------------
C   read in a time-range of data, replacing the selected in-core data
C   All data types are replaced.
C   Inputs:
C      IAN      I      Antenna to flag (if not ALLANT), main antenna in
C                      plots in any case
C      FLTIMS   I(2)   Time array indices of flag window
C   Output:
C      IERR     I      Error code: 0 -> okay
C-----------------------------------------------------------------------
      INTEGER   IAN, FLTIMS(2), IERR
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      LONGINT   LPTR, IPTR, JPTR, JPTR2
      INTEGER   IP1, IP2, IP, IA1, IA2, IA, NPT, LA, LT, IROW, ICL,
     *   TYPE, DIM(7), LR, IPRM, PLP1, PLP2, LF, LF1, LF2, LF0, I, J,
     *   LP, LP1, LP2
      REAL      VALUE(MAXIF), DR, DI, VA
      LOGICAL   WASDIF
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
      IF (DIMIF.GT.1) THEN
         LF0 = BIF
      ELSE
         LF0 = MAX (BIF, IFNOW)
         END IF
      IF ((ALLIF.EQ.1) .OR. (IFNOW.EQ.0)) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE IF (ALLIF.EQ.0) THEN
         LF1 = FGIFRS(1)
         LF2 = FGIFRS(2)
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
      IF ((ALLPOL) .OR. (POLNOW.EQ.0)) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (POLNOW.EQ.0) THEN
         LP1 = 1
         LP2 = 2
      ELSE
         LP1 = POLNOW
         LP2 = POLNOW
         END IF
      IF (ALLANT.EQ.0) THEN
         IA1 = 1
         IA2 = ANTMAX
      ELSE
         IA1 = IAN
         IA2 = IAN
         END IF
      NPT = 0
      DO 100 LF = LF1,LF2
      DO 100 IA = IA1,IA2
         J = 0
         DO 10 I = 1,NUMPLT
            IF (IA.EQ.PLTAN(I)) J = I
 10         CONTINUE
         IF ((LF.NE.IFNOW) .AND. (IFNOW.NE.0)) J = 0
         DO 50 LR = 1,MAXREC
            LPTR = DPTR + (LR - 1) * DIMREC
            LA = EDCORI(LPTR)
            LT = EDCORI(LPTR+1)
            IF ((IA.EQ.LA) .AND. (FLTIMS(1).LE.LT) .AND.
     *         (FLTIMS(2).GE.LT)) THEN
C                                       remove from plot
               IF (J.GT.0) THEN
                  DO 15 LP = LP1,LP2
                     CCOLOR = (LF - 1) * POLMAX + LP
                     PLP1 = DIMPRM + (LP-1) * DIMIF * DIMDAT - LF0 + LF
                     PLP2 = PLP1 + LTYPE * DIMIF
                     IF (DOCOMP) JPTR2 = PLP1 + LTYPE2 * DIMIF
                     DR = EDCORE(LPTR+PLP2)
                     IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
                     IF (DR.NE.FBLANK) THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(5), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE IF (J.EQ.1) THEN
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(4), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF (GRSEL(6).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(6), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
                     IF ((DOCOMP) .AND. (VA.NE.FBLANK) .AND. (J.EQ.1))
     *                  THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
 15                  CONTINUE
                  END IF
C                                       read in replacement
               WASDIF = .FALSE.
               IROW = EDCORI(LPTR+2)
               DO 30 IP = IP1,IP2
                  CCOLOR = (LF - 1) * POLMAX + LP
                  IPTR = LPTR + DIMPRM + (IP-1)*DIMIF*DIMDAT + LF - LF0
                  NPT = NPT + 1
                  DO 20 IPRM = 1,DIMDAT
                     ICL = 5 + (IP-1)*DIMDAT + IPRM
                     CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *                  VALUE, CDUMMY, IERR)
                     IF (IERR.GT.0) GO TO 990
                     IF (IERR.LT.0) GO TO 30
                     JPTR = IPTR + (IPRM-1) * DIMIF
                     IF (EDCORE(JPTR).NE.VALUE(LF)) WASDIF = .TRUE.
                     EDCORE(JPTR) = VALUE(LF)
 20                  CONTINUE
C                                       MB delay conversion
                  IF (MULTIB) THEN
                     DR = EDCORE(IPTR+DIMIF)
                     IF (DR.EQ.FBLANK) THEN
                        EDCORE(IPTR) = -1.0
                     ELSE
                        EDCORE(IPTR) = 1.0
                        END IF
C                                       convert Re/Im to Amp/phase
                  ELSE
                     DR = EDCORE(IPTR+DIMIF)
                     DI = EDCORE(IPTR+2*DIMIF)
                     IF ((DR.EQ.FBLANK) .OR. (DI.EQ.FBLANK) .OR.
     *                  ((DR.EQ.0.0) .AND. (DI.EQ.0.0))) THEN
                        EDCORE(IPTR) = -ABS (EDCORE(IPTR))
                     ELSE
                        EDCORE(IPTR+DIMIF) = SQRT (DR*DR + DI*DI)
                        EDCORE(IPTR+2*DIMIF) = ATAN2 (DI, DR)/DG2RAD
                        END IF
                     END IF
                  IF (WASDIF) WASFLG = MAX (0, WASFLG - 1)
 30               CONTINUE
               IERR = 0
C                                       plot
               IF (J.GT.0) THEN
                  DO 40 LP = LP1,LP2
                     CCOLOR = (LF - 1) * POLMAX + LP
                     PLP1 = DIMPRM + (LP-1) * DIMIF * DIMDAT - LF0 + LF
                     PLP2 = PLP1 + LTYPE * DIMIF
                     IF (DOCOMP) JPTR2 = PLP1 + LTYPE2 * DIMIF
                     DR = EDCORE(LPTR+PLP2)
                     IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
                     IF (DR.NE.FBLANK) THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(5), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE IF (J.EQ.1) THEN
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(4), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF (GRSEL(6).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(6), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
                     IF ((DOCOMP) .AND. (VA.NE.FBLANK) .AND. (J.EQ.1))
     *                  THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                             CALL EDPLTP (EXPLOT, GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (EXPLOT, GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
 40                  CONTINUE
                  END IF
               END IF
 50         CONTINUE
 100     CONTINUE
      WRITE (MSGTXT,1100) NPT
      CALL MSGWRT (2)
      IF (IFNOW.GE.0) THEN
         CALL EDFGAP (UVMAST, IERR)
         IF (IERR.EQ.0) FLAGED = .TRUE.
         END IF
      IERR = 0
      GO TO 999
C
 980  MSGTXT = 'EDG1SN: ERROR IN UPDATING TV FOR RE-READING DATA'
      CALL MSGWRT (6)
      GO TO 999
C
 990  WRITE (MSGTXT,1990) LR, IROW, COLLAB(ICL)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Replaced',I6,' samples from disk')
 1990 FORMAT ('ERROR READING REC',I5,' ROW',I6,' COL ',A)
      END
      SUBROUTINE EDG1SY (IAN, FLTIMS, IERR)
C-----------------------------------------------------------------------
C   read in a time-range of SY data, replacing the selected in-core
C   SY data.  All SY data types are replaced.
C   Inputs:
C      IAN      I      Antenna to flag (if not ALLANT), main antenna in
C                      plots in any case
C      FLTIMS   I(2)   Time array indices of flag window
C   Output:
C      IERR     I      Error code: 0 -> okay
C-----------------------------------------------------------------------
      INTEGER   IAN, FLTIMS(2), IERR
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      LONGINT   LPTR, IPTR, JPTR, JPTR2
      INTEGER   IP1, IP2, IP, IA1, IA2, IA, NPT, LA, LT, IROW, ICL,
     *   TYPE, DIM(7), LR, PLP1, PLP2, LF, LF1, LF2, LF0, I, J, LP,
     *   LP1, LP2, NPTB, IPOL, CALTYP
      REAL      VALUE(MAXIF), DR, VA, VS, VD, TC
      LOGICAL   WASDIF
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
      IF (DIMIF.GT.1) THEN
         LF0 = BIF
      ELSE
         LF0 = MAX (BIF, IFNOW)
         END IF
      IF ((ALLIF.EQ.1) .OR. (IFNOW.EQ.0)) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE IF (ALLIF.EQ.0) THEN
         LF1 = FGIFRS(1)
         LF2 = FGIFRS(2)
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
      IF ((ALLPOL) .OR. (POLNOW.EQ.0)) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (POLNOW.EQ.0) THEN
         LP1 = 1
         LP2 = POLMAX
      ELSE
         LP1 = POLNOW
         LP2 = POLNOW
         END IF
      IF (ALLANT.EQ.0) THEN
         IA1 = 1
         IA2 = ANTMAX
      ELSE
         IA1 = IAN
         IA2 = IAN
         END IF
      NPT = 0
      NPTB = 0
      DO 100 LF = LF1,LF2
      DO 100 IA = IA1,IA2
         J = 0
         DO 10 I = 1,NUMPLT
            IF (IA.EQ.PLTAN(I)) J = I
 10         CONTINUE
         IF ((LF.NE.IFNOW) .AND. (IFNOW.NE.0)) J = 0
         DO 50 LR = 1,MAXREC
            LPTR = DPTR + (LR - 1) * DIMREC
            LA = EDCORI(LPTR)
            LT = EDCORI(LPTR+1)
            CALTYP = EDCORI(LPTR+3)
            IF ((IA.EQ.LA) .AND. (FLTIMS(1).LE.LT) .AND.
     *         (FLTIMS(2).GE.LT)) THEN
C                                       remove from plot
               IF (J.GT.0) THEN
                  DO 15 LP = LP1,LP2
                     CCOLOR = (LF - 1) * POLMAX + LP
                     PLP1 = DIMPRM + (LP-1) * DIMIF * DIMDAT - LF0 + LF
                     PLP2 = PLP1 + LTYPE * DIMIF
                     IF (DOCOMP) JPTR2 = PLP1 + LTYPE2 * DIMIF
                     DR = EDCORE(LPTR+PLP2)
                     IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
                     IF (DR.NE.FBLANK) THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(5), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE IF (J.EQ.1) THEN
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(4), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF (GRSEL(6).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(6), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
                     IF ((DOCOMP) .AND. (VA.NE.FBLANK) .AND. (J.EQ.1))
     *                  THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
 15                  CONTINUE
                  END IF
C                                       read in replacement
               WASDIF = .FALSE.
               IROW = EDCORI(LPTR+2)
               DO 30 IPOL = IP1,IP2
                  IPTR = LPTR + DIMPRM + (IPOL-1)*DIMIF*DIMDAT + LF-LF0
                  NPT = NPT + 1
                  DO 25 IP = 1,3
                     ICL = 5 + IP + (IPOL-1) * 3
                     CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *                  VALUE, CDUMMY, IERR)
                     IF (IERR.GT.0) THEN
                        GO TO 990
                     ELSE IF (IERR.EQ.0) THEN
                        JPTR = IPTR + IP * DIMIF
                        IF (EDCORE(JPTR).NE.VALUE(LF)) WASDIF = .TRUE.
                        IF (VALUE(LF).EQ.FBLANK) THEN
                           EDCORE(IPTR) = 0.0
                           EDCORE(JPTR) = VALUE(LF)
                           EDCORE(JPTR+3*DIMIF) = FBLANK
                        ELSE
                           EDCORE(IPTR) = 1.0
                           EDCORE(JPTR) = VALUE(LF)
                           EDCORE(JPTR+3*DIMIF) = 0.0
                           END IF
                        IF (WASDIF) WASFLG = MAX (0, WASFLG - 1)
                        END IF
 25                  CONTINUE
C                                       ratio datum
                  VS = EDCORE(IPTR+2*DIMIF)
                  VD = EDCORE(IPTR+DIMIF)
                  IF (CALTYP.EQ.1) THEN
                     TC = TCAL(IPOL+2,LF,IA)
                  ELSE
                     TC = TCAL(IPOL,LF,IA)
                     END IF
                  IF ((VS.NE.FBLANK) .AND. (VD.NE.FBLANK) .AND.
     *               (VD.NE.0.0) .AND. (EDCORE(IPTR).GT.0.0)) THEN
                     IF (TC.NE.FBLANK) THEN
                        EDCORE(IPTR+4*DIMIF) = FBLANK
                        EDCORE(IPTR+8*DIMIF) = FBLANK
                     ELSE
                        EDCORE(IPTR+4*DIMIF) = VS / VD / 2.0 * TC
                        EDCORE(IPTR+8*DIMIF) = 0.0
                        END IF
                  ELSE
                     EDCORE(IPTR) = 0.0
                     EDCORE(IPTR+4*DIMIF) = FBLANK
                     EDCORE(IPTR+8*DIMIF) = FBLANK
                     END IF
 30               CONTINUE
               IERR = 0
C                                       plot
               IF (J.GT.0) THEN
                  DO 40 LP = LP1,LP2
                     CCOLOR = (LF - 1) * POLMAX + LP
                     PLP1 = DIMPRM + (LP-1) * DIMIF * DIMDAT - LF0 + LF
                     PLP2 = PLP1 + LTYPE * DIMIF
                     IF (DOCOMP) JPTR2 = PLP1 + LTYPE2 * DIMIF
                     DR = EDCORE(LPTR+PLP2)
                     IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
                     IF (DR.NE.FBLANK) THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(5), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                        ELSE IF (J.EQ.1) THEN
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(4), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                        ELSE
                           IF (GRSEL(6).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(6), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                           END IF
                        END IF
                     IF ((DOCOMP) .AND. (VA.NE.FBLANK) .AND. (J.EQ.1))
     *                  THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                             CALL EDPLTP (EXPLOT, GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                        ELSE
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (EXPLOT, GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                           END IF
                        END IF
 40                  CONTINUE
                  END IF
               END IF
 50         CONTINUE
 100     CONTINUE
      WRITE (MSGTXT,1100) NPT
      CALL MSGWRT (2)
      IF (NPT.GT.0) FLAGED = .TRUE.
      IF (NPTB.GT.0) THEN
         WRITE (MSGTXT,1101) NPTB
         CALL MSGWRT (7)
         END IF
      IF (IFNOW.GE.0) THEN
         CALL EDFGAP (UVMAST, IERR)
         IF (IERR.EQ.0) FLAGED = .TRUE.
         END IF
      IERR = 0
      GO TO 999
C
 980  MSGTXT = 'EDG1SY: ERROR IN UPDATING TV FOR RE-READING DATA'
      CALL MSGWRT (6)
      GO TO 999
C
 990  WRITE (MSGTXT,1990) LR, IROW, COLLAB(ICL)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Replaced',I6,' samples from disk')
 1101 FORMAT ('WARNING:',I6,' NEW POINTS FELL OFF PLOT - USE REPLOT')
 1990 FORMAT ('ERROR READING REC',I5,' ROW',I6,' COL ',A)
      END
      SUBROUTINE EDG1TY (IAN, FLTIMS, IERR)
C-----------------------------------------------------------------------
C   read in a time-range of TY data, replacing the selected in-core
C   TY data.  All TY data types are replaced.
C   Inputs:
C      IAN      I      Antenna to flag (if not ALLANT), main antenna in
C                      plots in any case
C      FLTIMS   I(2)   Time array indices of flag window
C   Output:
C      IERR     I      Error code: 0 -> okay
C-----------------------------------------------------------------------
      INTEGER   IAN, FLTIMS(2), IERR
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      LONGINT   LPTR, IPTR, JPTR, JPTR2
      INTEGER   IP1, IP2, IP, IA1, IA2, IA, NPT, LA, LT, IROW, ICL,
     *   TYPE, DIM(7), LR, PLP1, PLP2, LF, LF1, LF2, LF0, I, J, LP,
     *   LP1, LP2, NPTB
      REAL      VALUE(MAXIF), DR, VA
      LOGICAL   WASDIF
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
      IF (DIMIF.GT.1) THEN
         LF0 = BIF
      ELSE
         LF0 = MAX (BIF, IFNOW)
         END IF
      IF ((ALLIF.EQ.1) .OR. (IFNOW.EQ.0)) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE IF (ALLIF.EQ.0) THEN
         LF1 = FGIFRS(1)
         LF2 = FGIFRS(2)
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
      IF ((ALLPOL) .OR. (POLNOW.EQ.0)) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (POLNOW.EQ.0) THEN
         LP1 = 1
         LP2 = POLMAX
      ELSE
         LP1 = POLNOW
         LP2 = POLNOW
         END IF
      IF (ALLANT.EQ.0) THEN
         IA1 = 1
         IA2 = ANTMAX
      ELSE
         IA1 = IAN
         IA2 = IAN
         END IF
      NPT = 0
      NPTB = 0
      DO 100 LF = LF1,LF2
      DO 100 IA = IA1,IA2
         J = 0
         DO 10 I = 1,NUMPLT
            IF (IA.EQ.PLTAN(I)) J = I
 10         CONTINUE
         IF ((LF.NE.IFNOW) .AND. (IFNOW.NE.0)) J = 0
         DO 50 LR = 1,MAXREC
            LPTR = DPTR + (LR - 1) * DIMREC
            LA = EDCORI(LPTR)
            LT = EDCORI(LPTR+1)
            IF ((IA.EQ.LA) .AND. (FLTIMS(1).LE.LT) .AND.
     *         (FLTIMS(2).GE.LT)) THEN
C                                       remove from plot
               IF (J.GT.0) THEN
                  DO 15 LP = LP1,LP2
                     CCOLOR = (LF - 1) * POLMAX + LP
                     PLP1 = DIMPRM + (LP-1) * DIMIF * DIMDAT - LF0 + LF
                     PLP2 = PLP1 + LTYPE * DIMIF
                     IF (DOCOMP) JPTR2 = PLP1 + LTYPE2 * DIMIF
                     DR = EDCORE(LPTR+PLP2)
                     IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
                     IF (DR.NE.FBLANK) THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(5), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE IF (J.EQ.1) THEN
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(4), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF (GRSEL(6).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(6), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
                     IF ((DOCOMP) .AND. (VA.NE.FBLANK) .AND. (J.EQ.1))
     *                  THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
 15                  CONTINUE
                  END IF
C                                       read in replacement
               WASDIF = .FALSE.
               IROW = EDCORI(LPTR+2)
               DO 30 IP = IP1,IP2
                  IPTR = LPTR + DIMPRM + (IP-1)*DIMIF*DIMDAT + LF - LF0
                  NPT = NPT + 1
                  ICL = 5 + IP
                  CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *               VALUE, CDUMMY, IERR)
                  IF (IERR.GT.0) THEN
                     GO TO 990
                  ELSE IF (IERR.EQ.0) THEN
                     JPTR = IPTR + DIMIF
                     IF (ABS(VALUE(LF)-999.0).LT.0.1) VALUE(LF) = FBLANK
                     IF (EDCORE(JPTR).NE.VALUE(LF)) WASDIF = .TRUE.
                     IF (VALUE(LF).EQ.FBLANK) THEN
                        EDCORE(IPTR) = 0.0
                        EDCORE(JPTR) = VALUE(LF)
                        EDCORE(JPTR+DIMIF) = FBLANK
                     ELSE
                        EDCORE(IPTR) = 1.0
                        EDCORE(JPTR) = VALUE(LF)
                        EDCORE(JPTR+DIMIF) = 0.0
                        END IF
                     IF (WASDIF) WASFLG = MAX (0, WASFLG - 1)
                     END IF
 30               CONTINUE
               IERR = 0
C                                       plot
               IF (J.GT.0) THEN
                  DO 40 LP = LP1,LP2
                     CCOLOR = (LF - 1) * POLMAX + LP
                     PLP1 = DIMPRM + (LP-1) * DIMIF * DIMDAT - LF0 + LF
                     PLP2 = PLP1 + LTYPE * DIMIF
                     IF (DOCOMP) JPTR2 = PLP1 + LTYPE2 * DIMIF
                     DR = EDCORE(LPTR+PLP2)
                     IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
                     IF (DR.NE.FBLANK) THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(5), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                        ELSE IF (J.EQ.1) THEN
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(4), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                        ELSE
                           IF (GRSEL(6).GT.0) THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(6), LT,
     *                           TIME1, TIME2, DR, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                           END IF
                        END IF
                     IF ((DOCOMP) .AND. (VA.NE.FBLANK) .AND. (J.EQ.1))
     *                  THEN
                        IF (EDCORE(LPTR+PLP1).LT.0.0) THEN
                           IF (GRSEL(5).GT.0) THEN
                             CALL EDPLTP (EXPLOT, GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                        ELSE
                           IF (GRSEL(4).GT.0) THEN
                              CALL EDPLTP (EXPLOT, GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                           END IF
                        END IF
 40                  CONTINUE
                  END IF
               END IF
 50         CONTINUE
 100     CONTINUE
      WRITE (MSGTXT,1100) NPT
      CALL MSGWRT (2)
      IF (NPT.GT.0) FLAGED = .TRUE.
      IF (NPTB.GT.0) THEN
         WRITE (MSGTXT,1101) NPTB
         CALL MSGWRT (7)
         END IF
      IF (IFNOW.GE.0) THEN
         CALL EDFGAP (UVMAST, IERR)
         IF (IERR.EQ.0) FLAGED = .TRUE.
         END IF
      IERR = 0
      GO TO 999
C
 980  MSGTXT = 'EDG1TY: ERROR IN UPDATING TV FOR RE-READING DATA'
      CALL MSGWRT (6)
      GO TO 999
C
 990  WRITE (MSGTXT,1990) LR, IROW, COLLAB(ICL)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Replaced',I6,' samples from disk')
 1101 FORMAT ('WARNING:',I6,' NEW POINTS FELL OFF PLOT - USE REPLOT')
 1990 FORMAT ('ERROR READING REC',I5,' ROW',I6,' COL ',A)
      END
      SUBROUTINE EDGSIG (TYPE, WT, AMP, SIG)
C-----------------------------------------------------------------------
C   returns 1 sigma for given type, weight, amp
C   Inputs:
C      TYPE   I   Type of data: 1 amp, 2 phase, 3 Damp
C      WT     R   Data weight = 1/sigma**2 for amplitude
C      AMP    R   Data amplitude
C   Output:
C      SIG    R   1 sigma - 0 none
C-----------------------------------------------------------------------
      INTEGER  TYPE
      REAL     WT, AMP, SIG
C
      REAL     W
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SIG = 0.0
      IF ((WT.NE.FBLANK) .AND. (ABS(WT).GT.1.E-4)) THEN
         W = 1.0 / SQRT (ABS(WT))
         IF (TYPE.EQ.1) THEN
            SIG = W
         ELSE IF (TYPE.EQ.3) THEN
            SIG = W
         ELSE IF ((TYPE.EQ.2) .AND. (AMP.NE.FBLANK) .AND.
     *      (AMP.GT.1.E-4)) THEN
            SIG = W * RAD2DG / AMP
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDGTPT (ANT, TYPE, FLTIMS, FLFLUX, IERR)
C-----------------------------------------------------------------------
C   get the next non-flagged sample in the fltims, flflux area
C   Inputs:
C      ANT      I      Antenna number
C      TYPE     I      Data type
C      FLFLUX   R(2)   Lower, upper flux limits
C   In/Out:
C      FLTIMS   I(2)   in: Start, stop times of area
C                      out: start changed to point to sample found
C   Output:
C      IERR     I      Error code: < 0 no points found
C   This routine does NOT support POLNOW and IFNOW = 0.  They must be
C   set to real values.
C-----------------------------------------------------------------------
      INTEGER   ANT, TYPE, FLTIMS(2), IERR
      REAL      FLFLUX(2)
C
      LONGINT   LPTR, IPTR, JPTR
      INTEGER   LR1, LR2, LF, LR, LA, LT
      REAL      V
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
      LR1 = EDCORI(PPTR+FLTIMS(1)-1)
      LR2 = EDCORI(PPTR+FLTIMS(2)) - 1
      LR2 = MIN (LR2, MAXREC)
      IF (DIMIF.GT.1) THEN
         LF = MAX (BIF, IFNOW) - BIF
      ELSE
         LF = 0
         END IF
      IERR = -1
      IF (LR2.GE.LR1) THEN
         IPTR = DIMPRM + (POLNOW-1) * DIMIF * DIMDAT + LF
         JPTR = IPTR + TYPE * DIMIF
         DO 20 LR = LR1,LR2
            LPTR = DPTR + (LR - 1) * DIMREC
            LA = EDCORI(LPTR)
            LT = EDCORI(LPTR+1)
C                                       Only current baseline
C                                       in time range
            IF ((ANT.EQ.LA) .AND. (FLTIMS(1).LE.LT) .AND.
     *         (FLTIMS(2).GE.LT)) THEN
C                                       ignore weight/flagging
               V = EDCORE(LPTR+JPTR)
C                                       want it
               IF ((V.NE.FBLANK) .AND. (FLFLUX(1).LE.V) .AND.
     *            (FLFLUX(2).GE.V) .AND. (EDCORE(LPTR+IPTR).GT.0.0))
     *            THEN
                  FLTIMS(1) = LT
                  IERR = 0
                  GO TO 999
                  END IF
               END IF
 20         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDGTAP (ANT, TYPE, FLTIMS, FLFLUX, IERR)
C-----------------------------------------------------------------------
C   get the next sample in the fltims, flflux area even if flagged
C   Inputs:
C      ANT      I      Antenna number
C      TYPE     I      Data type
C      FLFLUX   R(2)   Lower, upper flux limits
C   In/Out:
C      FLTIMS   I(2)   in: Start, stop times of area
C                      out: start changed to point to sample found
C   Output:
C      IERR     I      Error code: < 0 no points found
C   This routine does NOT support POLNOW and IFNOW = 0.  They must be
C   set to real values.
C-----------------------------------------------------------------------
      INTEGER   ANT, TYPE, FLTIMS(2), IERR
      REAL      FLFLUX(2)
C
      LONGINT   LPTR, IPTR, JPTR
      INTEGER   LR1, LR2, LF, LR, LA, LT
      REAL      V
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
      LR1 = EDCORI(PPTR+FLTIMS(1)-1)
      LR2 = EDCORI(PPTR+FLTIMS(2)) - 1
      LR2 = MIN (LR2, MAXREC)
      IF (DIMIF.GT.1) THEN
         LF = MAX (BIF, IFNOW) - BIF
      ELSE
         LF = 0
         END IF
      IERR = -1
      IF (LR2.GE.LR1) THEN
         IPTR = DIMPRM + (POLNOW-1) * DIMIF * DIMDAT + LF
         JPTR = IPTR + TYPE * DIMIF
         DO 20 LR = LR1,LR2
            LPTR = DPTR + (LR - 1) * DIMREC
            LA = EDCORI(LPTR)
            LT = EDCORI(LPTR+1)
C                                       Only current baseline
C                                       in time range
            IF ((ANT.EQ.LA) .AND. (FLTIMS(1).LE.LT) .AND.
     *         (FLTIMS(2).GE.LT)) THEN
C                                       ignore weight/flagging
               V = EDCORE(LPTR+JPTR)
C                                       want it
               IF ((V.NE.FBLANK) .AND. (FLFLUX(1).LE.V) .AND.
     *            (FLFLUX(2).GE.V) .AND. (EDCORE(LPTR+IPTR).NE.0.0))
     *            THEN
                  FLTIMS(1) = LT
                  IERR = 0
                  GO TO 999
                  END IF
               END IF
 20         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDGTSN (IERR)
C-----------------------------------------------------------------------
C   Gets SN/CL data from OBJECT for specified IF.  When editing the SN
C   data themselves it write the file back again, but not when there is
C   an FC table and uv data are being edited with SN/CL values.
C   Output:
C      IERR     I       Error code: > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      LONGINT   LPTR, IPTR, JPTR
      INTEGER   I, NCL, ROW1, ROW2, IROW, IAN, TYPE, DIM(7), ITIM, IFQ,
     *   LF, LF1, LF2, PLF1, PLF2, IREC, IPOL, IPRM, ICL, SUBA, SOURC,
     *   GRNONE(5)
      REAL      A, P, W, DR, DI, VALUE(MAXIF)
      DOUBLE PRECISION TIME, LTIME
      CHARACTER COL1(5)*10, COL2(5)*10, COLRP(5)*10, CDUMMY*1
      DATA COLRP /'ANTENNA NO', 'TIME', 'FREQ ID', 'SUBARRAY',
     *   'SOURCE ID'/
      DATA COL1 /'WEIGHT 1', 'REAL1', 'IMAG1', 'DELAY 1', 'RATE 1'/
      DATA COL2 /'WEIGHT 2', 'REAL2', 'IMAG2', 'DELAY 2', 'RATE 2'/
C-----------------------------------------------------------------------
C                                       write message
      IF ((PIFNOW.GE.0) .AND. (MAXREC.GT.0) .AND. (.NOT.UVFLAG)) THEN
         IF (WASFLG.LT.0) THEN
            MSGTXT = 'No data changes to write back to disk'
         ELSE IF (WASFLG.EQ.0) THEN
            MSGTXT = 'Write data back to disk to be safe: change ' //
     *         'count ended at 0'
         ELSE
            WRITE (MSGTXT,1000) WASFLG
            END IF
         CALL MSGWRT (3)
         END IF
C                                       First time
      IF (MAXREC.LE.0) THEN
         IF (DTIME.LE.0.0) DTIME = 1.0 / 6000.0
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0*DTIME)
         DTIME = DTIME / (24 * 60.)
         GTIME = GTIME / (24 * 60.)
C                                       Get column pointers
         DIMPRM = 3
         DIMIF = EIF - BIF + 1
         DIMDAT = 3
         IF (DODELY) DIMDAT = 5
         IF (MULTIB) DIMDAT = 2
         DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
         NCL = 5
         COLLAB(1) = COLRP(1)
         COLLAB(2) = COLRP(2)
         COLLAB(3) = COLRP(3)
         COLLAB(4) = COLRP(4)
         COLLAB(5) = COLRP(5)
         IF (MULTIB) THEN
            NCL = NCL + 2 * POLMAX
            COLLAB(6) = 'MBDELAY1'
            COLLAB(7) = 'MBDELAY1'
            COLLAB(8) = 'MBDELAY2'
            COLLAB(9) = 'MBDELAY2'
         ELSE
            DO 10 I = 1,DIMDAT
               NCL = NCL + 1
               COLLAB(NCL) = COL1(I)
 10            CONTINUE
            END IF
         IF (POLMAX.EQ.2) THEN
            DO 15 I = 1,DIMDAT
               NCL = NCL + 1
               COLLAB(NCL) = COL2(I)
 15            CONTINUE
            END IF
         CALL TABCOL (TBEDIT, NCL, COLLAB, COLNUM, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'EDGTSN FAILS TO GET COLUMN NUMBERS'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       read through file to count rows
C                                       and to study times.
         ROW1 = ROWMAX + 1
         ROW2 = -1
         MAXREC = 0
         TIMEM = 0
         DO 30 IROW = 1,ROWMAX
            IREC = IROW
            ICL = 3
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (IERR.LT.0) GO TO 30
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 30
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SUBA,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((SUBA.GT.0) .AND. (SUBA.NE.SUBARR)) GO TO 30
            ICL = 1
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IAN,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            ICL = 2
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, TIME,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((IAN.LE.0) .OR. (IAN.GT.ANTMAX)) GO TO 30
            IF (ANTMLX.LT.ANTMAX) THEN
               DO 20 I = 1,ANTMLX
                  IF (IAN.EQ.ANTEN(I)) GO TO 25
 20               CONTINUE
               GO TO 30
               END IF
 25         IF (TIME.LT.TSTART) GO TO 30
            IF (TIME.GT.TEND) GO TO 40
            MAXREC = MAXREC + 1
            ROW1 = MIN (ROW1, IROW)
            ROW2 = MAX (ROW2, IROW)
            IF (TIMEM.EQ.0) THEN
               TIMEM = 5
               LTIME = TIME
            ELSE IF (TIME-LTIME.GT.2*GTIME) THEN
               TIMEM = TIMEM + 3
               LTIME = TIME
            ELSE IF (TIME-LTIME.GT.GTIME) THEN
               TIMEM = TIMEM + 2
               LTIME = TIME
            ELSE IF (TIME-LTIME.GT.DTIME) THEN
               TIMEM = TIMEM + 1
               LTIME = TIME
               END IF
 30         CONTINUE
         IERR = 0
C                                       finish the times
 40      TIMEM = TIMEM + 4
C                                       How big is the problem?
         I = MAXREC * DIMREC + 3 * (TIMEM + 100) + SCMAXS
         EDSIZE = I + 2048
         EDSIZE = (EDSIZE - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'EDGTSN', EDSIZE, EDCORE, EDIPTR, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'UNABLE TO ALLOCATE NEEDED MEMORY'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         EDSIZE = EDSIZE * 1024
         WRITE (MSGTXT,4000) I, EDIPTR
         CALL MSGWRT (9)
         ALLIN = I.LT.EDSIZE
         IF (.NOT.ALLIN) THEN
            DIMIF = 1
            DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
            I = MAXREC * DIMREC + 3 * (TIMEM + 100) + SCMAXS
            IF (I.GT.EDSIZE) THEN
               MSGTXT = 'ONE IF STILL TOO BIG FOR MEMORY: ' //
     *            'USE TIMERANG OR ANTENNAS'
               CALL MSGWRT (8)
               IERR = 8
               GO TO 999
            ELSE
               MSGTXT = 'Will do only one IF in memory at a time'
               END IF
         ELSE
            MSGTXT = 'All data will reside in memory'
            END IF
         CALL MSGWRT (3)
C                                       now fill in times, ants, ..
         SUPTR = TIMEM + 101 + EDIPTR
         PPTR = SUPTR + TIMEM + 100
         SPTR = PPTR + TIMEM + 100
         DPTR = SPTR + SCMAXS
         LPTR = DPTR
         IPTR = PPTR - 1
         JPTR = SUPTR - 1
         TIMEM = 0
         ITIM = 0
         MAXREC = 0
         DO 60 IROW = ROW1,ROW2
            IREC = IROW
            ICL = 3
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (IERR.LT.0) GO TO 60
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 60
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SUBA,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((SUBA.GT.0) .AND. (SUBA.NE.SUBARR)) GO TO 60
            ICL = 1
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IAN,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            ICL = 2
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, TIME,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((IAN.LE.0) .OR. (IAN.GT.ANTMAX)) GO TO 60
            IF (ANTMLX.LT.ANTMAX) THEN
               DO 45 I = 1,ANTMLX
                  IF (IAN.EQ.ANTEN(I)) GO TO 50
 45               CONTINUE
               GO TO 60
               END IF
 50         IF (TIME.LT.TSTART) GO TO 60
            IF (TIME.GT.TEND) GO TO 70
            ICL = 5
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SOURC,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            SOURC = MAX (1, SOURC)
            IF (TIMEM.EQ.0) THEN
               TIMEM = 5
               LTIME = TIME
               TIMES(EDIPTR+TIMEM) = TIME
               TIMES(EDIPTR+4) = TIME - 2*DTIME
               TIMES(EDIPTR+3) = TIME - 4*DTIME
               TIMES(EDIPTR+2) = TIME - 6*DTIME
               TIMES(EDIPTR+1) = TIME - 8*DTIME
               EDCORI(IPTR+1) = MAXREC + 1
               EDCORI(IPTR+2) = MAXREC + 1
               EDCORI(IPTR+3) = MAXREC + 1
               EDCORI(IPTR+4) = MAXREC + 1
               EDCORI(IPTR+5) = MAXREC + 1
               IPTR = IPTR + 5
               EDCORI(JPTR+1) = SOURC
               EDCORI(JPTR+2) = SOURC
               EDCORI(JPTR+3) = SOURC
               EDCORI(JPTR+4) = SOURC
               EDCORI(JPTR+5) = SOURC
               JPTR = JPTR + 5
            ELSE IF (TIME-LTIME.GT.2*GTIME) THEN
               TIMEM = TIMEM + 3
               TIMES(EDIPTR+TIMEM) = TIME
               TIMES(EDIPTR+TIMEM-1) = (2.*TIME + LTIME) / 3.0
               TIMES(EDIPTR+TIMEM-2) = (TIME + 2.*LTIME) / 3.0
               LTIME = TIME
               ITIM = ITIM + 2
               EDCORI(IPTR+1) = MAXREC + 1
               EDCORI(IPTR+2) = MAXREC + 1
               EDCORI(IPTR+3) = MAXREC + 1
               IPTR = IPTR + 3
               EDCORI(JPTR+1) = SOURC
               EDCORI(JPTR+2) = SOURC
               EDCORI(JPTR+3) = SOURC
               JPTR = JPTR + 3
            ELSE IF (TIME-LTIME.GT.GTIME) THEN
               TIMEM = TIMEM + 2
               TIMES(EDIPTR+TIMEM) = TIME
               TIMES(EDIPTR+TIMEM-1) = (TIME + LTIME) / 2.0
               LTIME = TIME
               ITIM = ITIM + 1
               EDCORI(IPTR+1) = MAXREC + 1
               EDCORI(IPTR+2) = MAXREC + 1
               IPTR = IPTR + 2
               EDCORI(JPTR+1) = SOURC
               EDCORI(JPTR+2) = SOURC
               JPTR = JPTR + 2
            ELSE IF (TIME-LTIME.GT.DTIME) THEN
               TIMEM = TIMEM + 1
               TIMES(EDIPTR+TIMEM) = TIME
               LTIME = TIME
               EDCORI(IPTR+1) = MAXREC + 1
               IPTR = IPTR + 1
               EDCORI(JPTR+1) = SOURC
               JPTR = JPTR + 1
               END IF
C                                       basic pointers

            EDCORI(LPTR+2) = IROW
            EDCORI(LPTR) = IAN
            EDCORI(LPTR+1) = TIMEM
            MAXREC = MAXREC + 1
            LPTR = LPTR + DIMREC
            MSAMPS(IAN) = MSAMPS(IAN) + 1
 60         CONTINUE
         IERR = 0
C                                       finish the times
 70      TIMES(EDIPTR+TIMEM+1) = TIMES(EDIPTR+TIMEM) + 2*DTIME
         TIMES(EDIPTR+TIMEM+2) = TIMES(EDIPTR+TIMEM) + 4*DTIME
         TIMES(EDIPTR+TIMEM+3) = TIMES(EDIPTR+TIMEM) + 6*DTIME
         TIMES(EDIPTR+TIMEM+4) = TIMES(EDIPTR+TIMEM) + 8*DTIME
         EDCORI(IPTR+1) = MAXREC + 1
         EDCORI(IPTR+2) = MAXREC + 1
         EDCORI(IPTR+3) = MAXREC + 1
         EDCORI(IPTR+4) = MAXREC + 1
         EDCORI(IPTR+5) = MAXREC + 1
         TIMEM = TIMEM + 4
         ITIM = TIMEM - 8 - ITIM
         WRITE (MSGTXT,1070) ITIM, TIMEM
         CALL MSGWRT (3)
         END IF
C                                       if allin and in the middle,
C                                       there is nothing to do
      IF ((.NOT.ALLIN) .OR. (PIFNOW.LT.0) .OR. (IFNOW.LT.0)) THEN
         IF (ALLIN) THEN
            LF1 = BIF
            LF2 = EIF
            PLF1 = LF1
            PLF2 = LF2
         ELSE
            LF1 = IFNOW
            LF2 = IFNOW
            PLF1 = PIFNOW
            PLF2 = PIFNOW
            END IF
         LPTR = DPTR
         DO 190 IREC = 1,MAXREC
            IROW = EDCORI(LPTR+2)
            DO 150 IPOL = 1,POLMAX
C                                       amp/phase -> real/imag
C                                       MB delay fix too
               IF ((PIFNOW.GE.0) .AND. (WASFLG.GE.0) .AND.
     *            (.NOT.UVFLAG)) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF -
     *               PLF1
                  IF (MULTIB) THEN
                     DO 105 LF = PLF1,PLF2
                        W = EDCORE(JPTR+LF)
                        A = EDCORE(JPTR+LF+DIMIF)
                        IF (W.LE.0.0) A = FBLANK
                        EDCORE(JPTR+LF+DIMIF) = A
                        EDCORE(JPTR+LF) = A
 105                    CONTINUE
                  ELSE
                     DO 110 LF = PLF1,PLF2
                        W = EDCORE(JPTR+LF)
                        A = EDCORE(JPTR+LF+DIMIF)
                        P = EDCORE(JPTR+LF+2*DIMIF)
                        IF ((A.EQ.FBLANK) .OR. (P.EQ.FBLANK) .OR.
     *                     ((A.EQ.0.0) .AND. (P.EQ.0.0))) THEN
                           EDCORE(JPTR+LF) = -ABS(W)
                        ELSE
                           EDCORE(JPTR+LF+DIMIF) = A * COS(DG2RAD*P)
                           EDCORE(JPTR+LF+2*DIMIF) = A * SIN(DG2RAD*P)
                           END IF
 110                    CONTINUE
                     END IF
                  END IF
               DO 130 IPRM = 1,DIMDAT
                  ICL = 5 + (IPOL-1)*DIMDAT + IPRM
                  CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *               VALUE, CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  IF (IERR.LT.0) GO TO 185
                  IF ((PIFNOW.GE.0) .AND. (WASFLG.GE.0) .AND.
     *               (.NOT.UVFLAG)) THEN
                     JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF +
     *                  (IPRM-1) * DIMIF - PLF1
                     DO 115 LF = PLF1,PLF2
                        VALUE(LF) = EDCORE(JPTR+LF)
 115                    CONTINUE
                     CALL TABDPT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *                  VALUE, CDUMMY, IERR)
                     IF (IERR.NE.0) GO TO 980
                     END IF
                  IF (IFNOW.GE.0) THEN
                     JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF +
     *                  (IPRM-1) * DIMIF - LF1
                     DO 120 LF = LF1,LF2
                        EDCORE(JPTR+LF) = VALUE(LF)
 120                    CONTINUE
                     END IF
 130              CONTINUE
C                                       real/imag -> amp/phase
C                                       MB delay fix too
               IF (IFNOW.GE.0) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF -
     *               LF1
                  IF (MULTIB) THEN
                     DO 135 LF = PLF1,PLF2
                        W = EDCORE(JPTR+LF)
                        A = EDCORE(JPTR+LF+DIMIF)
                        IF (A.EQ.FBLANK) THEN
                           EDCORE(JPTR+LF) = -1.0
                        ELSE
                           EDCORE(JPTR+LF) = +1.0
                           END IF
 135                    CONTINUE
                  ELSE
                     DO 140 LF = PLF1,PLF2
                        W = EDCORE(JPTR+LF)
                        DR = EDCORE(JPTR+LF+DIMIF)
                        DI = EDCORE(JPTR+LF+2*DIMIF)
                        IF ((DR.EQ.FBLANK) .OR. (DI.EQ.FBLANK) .OR.
     *                     ((DR.EQ.0.0) .AND. (DI.EQ.0.0))) THEN
                           EDCORE(JPTR+LF) = -ABS(W)
                        ELSE
                           EDCORE(JPTR+LF+DIMIF) = SQRT (DR*DR + DI*DI)
                           EDCORE(JPTR+LF+2*DIMIF) = ATAN2 (DI, DR)
     *                        /DG2RAD
                           END IF
 140                    CONTINUE
                     END IF
                  END IF
 150           CONTINUE
 185        LPTR = LPTR + DIMREC
 190        CONTINUE
         WASFLG = -1
         IF (IFNOW.GE.0) THEN
            CALL EDFGAP (UVMAST, IERR)
            IF (IERR.EQ.0) FLAGED = .TRUE.
            END IF
         END IF
C                                       redo flags
      IF ((.NOT.ALLIN) .AND. (IFNOW.GT.0) .AND. (IFWASN(IFNOW)) .AND.
     *   (UVFLAG)) THEN
         CALL COPY (5, GRSEL(4), GRNONE)
         CALL FILL (5, 0, GRSEL(4))
         CALL EDFCDO (IERR)
         CALL COPY (5, GRNONE, GRSEL(4))
         END IF
      GO TO 999
C                                       write error
 980  WRITE (MSGTXT,1980) IREC, IROW, COLLAB(ICL)
      CALL MSGWRT (7)
      GO TO 999
 990  WRITE (MSGTXT,1990) IREC, IROW, COLLAB(ICL)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Write data back to disk: change count =',I8)
 1070 FORMAT ('Found',I6,' times, stored on a time axis of length',I6)
 1980 FORMAT ('EDGTSN: ERROR WRITING REC',I5,' ROW',I6,' COL ',A)
 1990 FORMAT ('EDGTSN: ERROR READING REC',I5,' ROW',I6,' COL ',A)
 4000 FORMAT ('Allocate ',I12,' words at',I18)
      END
      SUBROUTINE EDGTTY (IERR)
C-----------------------------------------------------------------------
C   Gets TY data from OBJECT for specified IF  When editing the TY
C   data themselves it writeS the file back again, but not when there is
C   an FC table and uv data are being edited with TY values.
C   Output:
C      IERR     I       Error code: > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      LONGINT   LPTR, IPTR, JPTR, KPTR
      INTEGER   I, NCL, ROW1, ROW2, IROW, IAN, TYPE, DIM(7), ITIM, IFQ,
     *   LF, LF1, LF2, PLF1, PLF2, IREC, IPOL, ICL, ISUB, SOURC,
     *   GRNONE(5), GOODA, GOODS
      REAL      VALUE(MAXIF), TIME, LTIME
      CHARACTER COL1(2)*10, COL2(2)*10, COLRP(5)*10, CDUMMY*1
      DATA COLRP /'ANTENNA NO', 'TIME', 'FREQ ID', 'SUBARRAY',
     *   'SOURCE ID'/
      DATA COL1 /'TANT 1', 'TSYS 1'/
      DATA COL2 /'TANT 2', 'TSYS 2'/
C-----------------------------------------------------------------------
C                                       write message
      IF ((PIFNOW.GE.0) .AND. (MAXREC.GT.0) .AND. (.NOT.UVFLAG)) THEN
         IF (WASFLG.LT.0) THEN
            MSGTXT = 'No data changes to write back to disk'
         ELSE IF (WASFLG.EQ.0) THEN
            MSGTXT = 'Write data back to disk to be safe: change ' //
     *         'count ended at 0'
         ELSE
            WRITE (MSGTXT,1000) WASFLG
            END IF
         CALL MSGWRT (3)
         END IF
C                                       First time
      IF (MAXREC.LE.0) THEN
         IF (DTIME.LE.0.0) DTIME = 1.0 / 6000.0
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0 * DTIME)
         DTIME = DTIME / (24 * 60.)
         GTIME = GTIME / (24 * 60.)
C                                       Get column pointers
         DIMPRM = 3
         DIMIF = EIF - BIF + 1
         DIMDAT = 3
         DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
         COLLAB(1) = COLRP(1)
         COLLAB(2) = COLRP(2)
         COLLAB(3) = COLRP(3)
         COLLAB(4) = COLRP(4)
         COLLAB(5) = COLRP(5)
         COLLAB(6) = COL1(1)
         IF (POLMAX.EQ.2) THEN
            COLLAB(7) = COL2(1)
            COLLAB(8) = COL1(2)
            COLLAB(9) = COL2(2)
         ELSE
            COLLAB(7) = COL1(2)
            END IF
         NCL = 5 + 2 * POLMAX
         CALL TABCOL (TBEDIT, NCL, COLLAB, COLNUM, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'EDGTTY FAILS TO GET COLUMN NUMBERS'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       read through file to count rows
C                                       and to study times.
         ROW1 = ROWMAX + 1
         ROW2 = -1
         MAXREC = 0
         TIMEM = 0
         GOODA = 0
         GOODS = 0
         DO 30 IROW = 1,ROWMAX
            IREC = IROW
            ICL = 3
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.GT.0) GO TO 990
            IF (IERR.LT.0) GO TO 30
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 30
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, ISUB,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ISUB.GT.0) .AND. (ISUB.NE.SUBARR)) GO TO 30
            ICL = 1
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IAN,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            ICL = 2
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, TIME,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((IAN.LE.0) .OR. (IAN.GT.ANTMAX)) GO TO 30
            IF (ANTMLX.LT.ANTMAX) THEN
               DO 10 I = 1,ANTMLX
                  IF (IAN.EQ.ANTEN(I)) GO TO 15
 10               CONTINUE
               GO TO 30
               END IF
 15         IF (TIME.GT.TEND) GO TO 40
            IF (TIME.GE.TSTART) THEN
               MAXREC = MAXREC + 1
               ROW1 = MIN (ROW1, IROW)
               ROW2 = MAX (ROW2, IROW)
               IF (TIMEM.EQ.0) THEN
                  TIMEM = 5
                  LTIME = TIME
               ELSE IF (TIME-LTIME.GT.2*GTIME) THEN
                  TIMEM = TIMEM + 3
                  LTIME = TIME
               ELSE IF (TIME-LTIME.GT.GTIME) THEN
                  TIMEM = TIMEM + 2
                  LTIME = TIME
               ELSE IF (TIME-LTIME.GT.DTIME) THEN
                  TIMEM = TIMEM + 1
                  LTIME = TIME
                  END IF
C                                       Are the data good?
               DO 20 IPOL = 1,POLMAX
                  ICL = 5 + IPOL
                  CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *               VALUE, CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  DO 19 LF = BIF,EIF
                     IF (ABS(VALUE(LF)-999.0).LT.0.1) VALUE(LF) = FBLANK
                     IF (VALUE(LF).NE.FBLANK) GOODA = GOODA + 1
 19                  CONTINUE
 20               CONTINUE
               ICL = 6 + POLMAX
               DO 25 IPOL = 1,POLMAX
                  CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *               VALUE, CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  ICL = ICL + 1
                  DO 24 LF = BIF,EIF
                     IF (ABS(VALUE(LF)-999.0).LT.0.1) VALUE(LF) = FBLANK
                     IF (VALUE(LF).NE.FBLANK) GOODS = GOODS + 1
 24                  CONTINUE
 25               CONTINUE
               END IF
 30         CONTINUE
         IERR = 0
C                                       finish the times
 40      TIMEM = TIMEM + 4
         IF (GOODA.GT.0.7*GOODS) THEN
            MSGTXT = 'Using Tant columns rather than Tsys columns'
         ELSE
            MSGTXT = 'Tant columns are not good, using Tsys columns'
            IF (POLMAX.EQ.1) THEN
               COLNUM(6) = COLNUM(7)
               COLLAB(6) = COLLAB(7)
            ELSE
               COLNUM(6) = COLNUM(8)
               COLLAB(6) = COLLAB(8)
               COLNUM(7) = COLNUM(9)
               COLLAB(7) = COLLAB(9)
               END IF
            END IF
         CALL MSGWRT (3)
C                                       How big is the problem?
         I = MAXREC * DIMREC + 3 * (TIMEM + 100) + SCMAXS
         EDSIZE = I + 2048
         EDSIZE = (EDSIZE - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'EDGTTY', EDSIZE, EDCORE, EDIPTR, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'UNABLE TO ALLOCATE NEEDED MEMORY'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         EDSIZE = EDSIZE* 1024
         WRITE (MSGTXT,4000) I, EDIPTR
         CALL MSGWRT (9)
4000  FORMAT ('Allocate ',I12,' words at',I18)
         ALLIN = I.LT.EDSIZE
         IF (.NOT.ALLIN) THEN
            DIMIF = 1
            DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
            I = MAXREC * DIMREC + 3 * (TIMEM + 100) + SCMAXS
            IF (I.GT.EDSIZE) THEN
               MSGTXT = 'ONE IF STILL TOO BIG FOR MEMORY: ' //
     *            'USE TIMERANG OR ANTENNAS'
               CALL MSGWRT (8)
               IERR = 8
               GO TO 999
            ELSE
               MSGTXT = 'Will do only one IF in memory at a time'
               END IF
         ELSE
            MSGTXT = 'All data will reside in memory'
            END IF
         CALL MSGWRT (3)
C                                       now fill in times, ants, ..
         SUPTR = TIMEM + 101 + EDIPTR
         PPTR = SUPTR + TIMEM + 100
         SPTR = PPTR + TIMEM + 100
         DPTR = SPTR + SCMAXS
         LPTR = DPTR
         IPTR = PPTR - 1
         JPTR = SUPTR - 1
         TIMEM = 0
         ITIM = 0
         MAXREC = 0
         DO 60 IROW = ROW1,ROW2
            IREC = IROW
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, ISUB,
     *         CDUMMY, IERR)
            IF (IERR.GT.0) GO TO 990
            IF (IERR.LT.0) GO TO 60
            IF ((ISUB.GT.0) .AND. (ISUB.NE.SUBARR)) GO TO 60
            ICL = 3
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 60
            ICL = 1
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IAN,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            ICL = 2
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, TIME,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((IAN.LE.0) .OR. (IAN.GT.ANTMAX)) GO TO 60
            IF (ANTMLX.LT.ANTMAX) THEN
               DO 45 I = 1,ANTMLX
                  IF (IAN.EQ.ANTEN(I)) GO TO 50
 45               CONTINUE
               GO TO 60
               END IF
 50         IF (TIME.LT.TSTART) GO TO 60
            IF (TIME.GT.TEND) GO TO 70
            ICL = 5
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SOURC,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            SOURC = MAX (1, SOURC)
            IF (TIMEM.EQ.0) THEN
               TIMEM = 5
               LTIME = TIME
               TIMES(EDIPTR+TIMEM) = TIME
               TIMES(EDIPTR+4) = TIME - 2*DTIME
               TIMES(EDIPTR+3) = TIME - 4*DTIME
               TIMES(EDIPTR+2) = TIME - 6*DTIME
               TIMES(EDIPTR+1) = TIME - 8*DTIME
               EDCORI(IPTR+1) = MAXREC + 1
               EDCORI(IPTR+2) = MAXREC + 1
               EDCORI(IPTR+3) = MAXREC + 1
               EDCORI(IPTR+4) = MAXREC + 1
               EDCORI(IPTR+5) = MAXREC + 1
               IPTR = IPTR + 5
               EDCORI(JPTR+1) = SOURC
               EDCORI(JPTR+2) = SOURC
               EDCORI(JPTR+3) = SOURC
               EDCORI(JPTR+4) = SOURC
               EDCORI(JPTR+5) = SOURC
               JPTR = JPTR + 5
            ELSE IF (TIME-LTIME.GT.2*GTIME) THEN
               TIMEM = TIMEM + 3
               TIMES(EDIPTR+TIMEM) = TIME
               TIMES(EDIPTR+TIMEM-1) = (2.*TIME + LTIME) / 3.0
               TIMES(EDIPTR+TIMEM-2) = (TIME + 2.*LTIME) / 3.0
               LTIME = TIME
               ITIM = ITIM + 2
               EDCORI(IPTR+1) = MAXREC + 1
               EDCORI(IPTR+2) = MAXREC + 1
               EDCORI(IPTR+3) = MAXREC + 1
               IPTR = IPTR + 3
               EDCORI(JPTR+1) = SOURC
               EDCORI(JPTR+2) = SOURC
               EDCORI(JPTR+3) = SOURC
               JPTR = JPTR + 3
            ELSE IF (TIME-LTIME.GT.GTIME) THEN
               TIMEM = TIMEM + 2
               TIMES(EDIPTR+TIMEM) = TIME
               TIMES(EDIPTR+TIMEM-1) = (TIME + LTIME) / 2.0
               LTIME = TIME
               ITIM = ITIM + 1
               EDCORI(IPTR+1) = MAXREC + 1
               EDCORI(IPTR+2) = MAXREC + 1
               IPTR = IPTR + 2
               EDCORI(JPTR+1) = SOURC
               EDCORI(JPTR+2) = SOURC
               JPTR = JPTR + 2
            ELSE IF (TIME-LTIME.GT.DTIME) THEN
               TIMEM = TIMEM + 1
               TIMES(EDIPTR+TIMEM) = TIME
               LTIME = TIME
               EDCORI(IPTR+1) = MAXREC + 1
               IPTR = IPTR + 1
               EDCORI(JPTR+1) = SOURC
               JPTR = JPTR + 1
               END IF
C                                       basic pointers
            EDCORI(LPTR+2) = IROW
            EDCORI(LPTR) = IAN
            EDCORI(LPTR+1) = TIMEM
            MAXREC = MAXREC + 1
            LPTR = LPTR + DIMREC
            MSAMPS(IAN) = MSAMPS(IAN) + 1
 60         CONTINUE
         IERR = 0
C                                       finish the times
 70      TIMES(EDIPTR+TIMEM+1) = TIMES(EDIPTR+TIMEM) + 2*DTIME
         TIMES(EDIPTR+TIMEM+2) = TIMES(EDIPTR+TIMEM) + 4*DTIME
         TIMES(EDIPTR+TIMEM+3) = TIMES(EDIPTR+TIMEM) + 6*DTIME
         TIMES(EDIPTR+TIMEM+4) = TIMES(EDIPTR+TIMEM) + 8*DTIME
         EDCORI(IPTR+1) = MAXREC + 1
         EDCORI(IPTR+2) = MAXREC + 1
         EDCORI(IPTR+3) = MAXREC + 1
         EDCORI(IPTR+4) = MAXREC + 1
         EDCORI(IPTR+5) = MAXREC + 1
         TIMEM = TIMEM + 4
         ITIM = TIMEM - 8 - ITIM
         WRITE (MSGTXT,1070) ITIM, TIMEM
         CALL MSGWRT (3)
         END IF
C                                       All calls:
C                                       if allin and in the middle,
C                                       there is nothing to do
      IF ((.NOT.ALLIN) .OR. (PIFNOW.LT.0) .OR. (IFNOW.LT.0)) THEN
         IF (ALLIN) THEN
            LF1 = BIF
            LF2 = EIF
            PLF1 = LF1
            PLF2 = LF2
         ELSE
            LF1 = IFNOW
            LF2 = IFNOW
            PLF1 = PIFNOW
            PLF2 = PIFNOW
            END IF
         LPTR = DPTR
         DO 190 IREC = 1,MAXREC
            IROW = EDCORI(LPTR+2)
C                                       weight = 1 for flag
            DO 150 IPOL = 1,POLMAX
               ICL = 5 + IPOL
               CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, VALUE,
     *            CDUMMY, IERR)
               IF (IERR.GT.0) GO TO 990
               IF (IERR.LT.0) GO TO 185
C                                       write data back to disk
               IF ((PIFNOW.GE.0) .AND. (WASFLG.GE.0) .AND.
     *            (.NOT.UVFLAG)) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF -
     *               PLF1
                  KPTR = JPTR + DIMIF
                  DO 110 LF = PLF1,PLF2
                     IF (EDCORE(JPTR+LF).LE.0.0) THEN
                        VALUE(LF) = FBLANK
                     ELSE
                        VALUE(LF) = EDCORE(KPTR+LF)
                        END IF
 110                 CONTINUE
                  CALL TABDPT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *               VALUE, CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
C                                       load new data to core
               IF (IFNOW.GE.0) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF - LF1
                  KPTR = JPTR + DIMIF
                  DO 120 LF = LF1,LF2
                     IF (ABS(VALUE(LF)-999.0).LT.0.1) VALUE(LF) = FBLANK
                     IF (VALUE(LF).EQ.FBLANK) THEN
                        EDCORE(JPTR+LF) = 0.0
                        EDCORE(KPTR+LF) = VALUE(LF)
                        EDCORE(KPTR+LF+DIMIF) = FBLANK
                     ELSE
                        EDCORE(JPTR+LF) = 1.0
                        EDCORE(KPTR+LF) = VALUE(LF)
                        EDCORE(KPTR+LF+DIMIF) = 0.0
                        END IF
 120                 CONTINUE
                  END IF
 150           CONTINUE
 185        LPTR = LPTR + DIMREC
 190        CONTINUE
         WASFLG = -1
         IF (IFNOW.GE.0) THEN
            CALL EDFGAP (UVMAST, IERR)
            IF (IERR.EQ.0) FLAGED = .TRUE.
            END IF
         END IF
C                                       redo flags
      IF ((.NOT.ALLIN) .AND. (IFNOW.GT.0) .AND. (IFWASN(IFNOW))) THEN
         CALL COPY (5, GRSEL(4), GRNONE)
         CALL FILL (5, 0, GRSEL(4))
         CALL EDFCDO (IERR)
         CALL COPY (5, GRNONE, GRSEL(4))
         END IF
      GO TO 999
C                                       write error
 990  WRITE (MSGTXT,1990) IREC, IROW, COLLAB(ICL)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Write data back to disk: change count =',I8)
 1070 FORMAT ('Found',I6,' times, stored on a time axis of length',I6)
 1990 FORMAT ('EDGTTY: ERROR READING REC',I5,' ROW',I6,' COL ',A)
      END
      SUBROUTINE EDGTSY (IERR)
C-----------------------------------------------------------------------
C   Gets SY data from OBJECT for specified IF  When editing the SY
C   data themselves it writes the file back again, but not when there is
C   an FC table and UV data are being edited with SY values.
C   Output:
C      IERR     I       Error code: > 0 => quit
C-----------------------------------------------------------------------
C   AP memory allocation:
C      Begin   Number     What
C          1    TIMEM     Times in floating days
C       SUPTR   TIMEM     Source numbers
C       PPTR    TIMEM     Record number of first data sample at time i
C                         as addresses in data area (EDCORE(DPTR++))
C       SPTR    6000+     Scratch area (e.g. 3x2000 for smoothing)
C                         or larger in TYEDT is TIMEM > 6000 for source#
C       DPTR   MAXREC     Data records each DIMREC words long
C                            DIMPRM + Array(1-DIMIF, 1-DIMDAT, 1-POLMAX)
C                            DIMPRM = antenna, time counter, table row#,
C                                     cal type
C                            DIMIF = EIF-BIF+1  or 1 depending on core
C                            DIMDAT = 9
C                               wt (0,1), Pdif, Psum, Pgain, Psys
C                               + 4 more for running differences
C                            POLMAX = 1 or 2
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      LONGINT   LPTR, IPTR, JPTR, KPTR
      INTEGER   I, NCL, ROW1, ROW2, IROW, IAN, TYPE, DIM(7), ITIM, IFQ,
     *   LF, LF1, LF2, PLF1, PLF2, IREC, IPOL, ICL, ISUB, SOURC, IP,
     *   GRNONE(5), GOODA, GOODS, LA, CALTYP
      REAL      VALUE(MAXIF), VS, VD, TC
      DOUBLE PRECISION TIME, LTIME
      CHARACTER COL1(3)*10, COL2(3)*10, COLRP(6)*10, CDUMMY*1
      DATA COLRP /'ANTENNA NO', 'TIME', 'FREQ ID', 'SUBARRAY',
     *   'SOURCE ID', 'CAL TYPE'/
      DATA COL1 /'POWER DIF1', 'POWER SUM1', 'POST GAIN1'/
      DATA COL2 /'POWER DIF2', 'POWER SUM2', 'POST GAIN2'/
C-----------------------------------------------------------------------
C                                       write message
      IF ((PIFNOW.GE.0) .AND. (MAXREC.GT.0) .AND. (.NOT.UVFLAG)) THEN
         IF (WASFLG.LT.0) THEN
            MSGTXT = 'No data changes to write back to disk'
         ELSE IF (WASFLG.EQ.0) THEN
            MSGTXT = 'Write data back to disk to be safe: change ' //
     *         'count ended at 0'
         ELSE
            WRITE (MSGTXT,1000) WASFLG
            END IF
         CALL MSGWRT (3)
         END IF
C                                       First time
      CALTYP = 0
      IF (MAXREC.LE.0) THEN
         IF (DTIME.LE.0.0) DTIME = 1.0 / 6000.0
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0 * DTIME)
         DTIME = DTIME / (24 * 60.)
         GTIME = GTIME / (24 * 60.)
C                                       Get column pointers
         DIMPRM = 4
         DIMIF = EIF - BIF + 1
         DIMDAT = 9
         DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
         COLLAB(1) = COLRP(1)
         COLLAB(2) = COLRP(2)
         COLLAB(3) = COLRP(3)
         COLLAB(4) = COLRP(4)
         COLLAB(5) = COLRP(5)
         COLLAB(6) = COL1(1)
         COLLAB(7) = COL1(2)
         COLLAB(8) = COL1(3)
         COLLAB(9) = COL2(1)
         COLLAB(10) = COL2(2)
         COLLAB(11) = COL2(3)
         COLLAB(12) = COLRP(6)
         CALL TABCOL (TBEDIT, 1, COLLAB(12), COLNUM(12), IERR)
         IF (IERR.NE.0) THEN
            COLNUM(12) = -1
            IERR = 0
            END IF
         NCL = 5 + 3 * POLMAX
         CALL TABCOL (TBEDIT, NCL, COLLAB, COLNUM, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'EDGTSY FAILS TO GET COLUMN NUMBERS'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       read through file to count rows
C                                       and to study times.
         ROW1 = ROWMAX + 1
         ROW2 = -1
         MAXREC = 0
         TIMEM = 0
         GOODA = 0
         GOODS = 0
         DO 30 IROW = 1,ROWMAX
            IREC = IROW
            ICL = 3
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.GT.0) GO TO 990
            IF (IERR.LT.0) GO TO 30
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 30
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, ISUB,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ISUB.GT.0) .AND. (ISUB.NE.SUBARR)) GO TO 30
            ICL = 1
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IAN,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            ICL = 2
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, TIME,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((IAN.LE.0) .OR. (IAN.GT.ANTMAX)) GO TO 30
            IF (ANTMLX.LT.ANTMAX) THEN
               DO 10 I = 1,ANTMLX
                  IF (IAN.EQ.ANTEN(I)) GO TO 15
 10               CONTINUE
               GO TO 30
               END IF
 15         IF (TIME.GT.TEND) GO TO 40
            IF (TIME.GE.TSTART) THEN
               MAXREC = MAXREC + 1
               ROW1 = MIN (ROW1, IROW)
               ROW2 = MAX (ROW2, IROW)
               IF (TIMEM.EQ.0) THEN
                  TIMEM = 5
                  LTIME = TIME
               ELSE IF (TIME-LTIME.GT.2*GTIME) THEN
                  TIMEM = TIMEM + 3
                  LTIME = TIME
               ELSE IF (TIME-LTIME.GT.GTIME) THEN
                  TIMEM = TIMEM + 2
                  LTIME = TIME
               ELSE IF (TIME-LTIME.GT.DTIME) THEN
                  TIMEM = TIMEM + 1
                  LTIME = TIME
                  END IF
               END IF
 30         CONTINUE
         IERR = 0
C                                       finish the times
 40      TIMEM = TIMEM + 4
C                                       How big is the problem?
         I = MAXREC * DIMREC + 3 * (TIMEM + 100) + SCMAXS
         EDSIZE = I + 2048
         EDSIZE = (EDSIZE - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'EDGTSY', EDSIZE, EDCORE, EDIPTR, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'UNABLE TO ALLOCATE NEEDED MEMORY'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         EDSIZE = EDSIZE * 1024
         WRITE (MSGTXT,4000) I, EDIPTR
         CALL MSGWRT (9)
         ALLIN = I.LT.EDSIZE
         IF (.NOT.ALLIN) THEN
            DIMIF = 1
            DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
            I = MAXREC * DIMREC + 3 * (TIMEM + 100) + SCMAXS
            IF (I.GT.EDSIZE) THEN
               MSGTXT = 'ONE IF STILL TOO BIG FOR MEMORY: ' //
     *            'USE TIMERANG OR ANTENNAS'
               CALL MSGWRT (8)
               IERR = 8
               GO TO 999
            ELSE
               MSGTXT = 'Will do only one IF in memory at a time'
               END IF
         ELSE
            MSGTXT = 'All data will reside in memory'
            END IF
         CALL MSGWRT (3)
C                                       now fill in times, ants, ..
         SUPTR = TIMEM + 101 + EDIPTR
         PPTR = SUPTR + TIMEM + 100
         SPTR = PPTR + TIMEM + 100
         DPTR = SPTR + SCMAXS
         LPTR = DPTR
         IPTR = PPTR - 1
         JPTR = SUPTR - 1
         TIMEM = 0
         ITIM = 0
         MAXREC = 0
         DO 60 IROW = ROW1,ROW2
            IREC = IROW
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, ISUB,
     *         CDUMMY, IERR)
            IF (IERR.GT.0) GO TO 990
            IF (IERR.LT.0) GO TO 60
            IF ((ISUB.GT.0) .AND. (ISUB.NE.SUBARR)) GO TO 60
            ICL = 3
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 60
            ICL = 1
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IAN,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            ICL = 2
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, TIME,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((IAN.LE.0) .OR. (IAN.GT.ANTMAX)) GO TO 60
            IF (ANTMLX.LT.ANTMAX) THEN
               DO 45 I = 1,ANTMLX
                  IF (IAN.EQ.ANTEN(I)) GO TO 50
 45               CONTINUE
               GO TO 60
               END IF
 50         IF (TIME.LT.TSTART) GO TO 60
            IF (TIME.GT.TEND) GO TO 70
            ICL = 5
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SOURC,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            SOURC = MAX (1, SOURC)
            IF (COLNUM(12).GT.0) THEN
               ICL = 12
               CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *            CALTYP, CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF (TIMEM.EQ.0) THEN
               TIMEM = 5
               LTIME = TIME
               TIMES(EDIPTR+TIMEM) = TIME
               TIMES(EDIPTR+4) = TIME - 2*DTIME
               TIMES(EDIPTR+3) = TIME - 4*DTIME
               TIMES(EDIPTR+2) = TIME - 6*DTIME
               TIMES(EDIPTR+1) = TIME - 8*DTIME
               EDCORI(IPTR+1) = MAXREC + 1
               EDCORI(IPTR+2) = MAXREC + 1
               EDCORI(IPTR+3) = MAXREC + 1
               EDCORI(IPTR+4) = MAXREC + 1
               EDCORI(IPTR+5) = MAXREC + 1
               IPTR = IPTR + 5
               EDCORI(JPTR+1) = SOURC
               EDCORI(JPTR+2) = SOURC
               EDCORI(JPTR+3) = SOURC
               EDCORI(JPTR+4) = SOURC
               EDCORI(JPTR+5) = SOURC
               JPTR = JPTR + 5
            ELSE IF (TIME-LTIME.GT.2*GTIME) THEN
               TIMEM = TIMEM + 3
               TIMES(EDIPTR+TIMEM) = TIME
               TIMES(EDIPTR+TIMEM-1) = (2.*TIME + LTIME) / 3.0
               TIMES(EDIPTR+TIMEM-2) = (TIME + 2.*LTIME) / 3.0
               LTIME = TIME
               ITIM = ITIM + 2
               EDCORI(IPTR+1) = MAXREC + 1
               EDCORI(IPTR+2) = MAXREC + 1
               EDCORI(IPTR+3) = MAXREC + 1
               IPTR = IPTR + 3
               EDCORI(JPTR+1) = SOURC
               EDCORI(JPTR+2) = SOURC
               EDCORI(JPTR+3) = SOURC
               JPTR = JPTR + 3
            ELSE IF (TIME-LTIME.GT.GTIME) THEN
               TIMEM = TIMEM + 2
               TIMES(EDIPTR+TIMEM) = TIME
               TIMES(EDIPTR+TIMEM-1) = (TIME + LTIME) / 2.0
               LTIME = TIME
               ITIM = ITIM + 1
               EDCORI(IPTR+1) = MAXREC + 1
               EDCORI(IPTR+2) = MAXREC + 1
               IPTR = IPTR + 2
               EDCORI(JPTR+1) = SOURC
               EDCORI(JPTR+2) = SOURC
               JPTR = JPTR + 2
            ELSE IF (TIME-LTIME.GT.DTIME) THEN
               TIMEM = TIMEM + 1
               TIMES(EDIPTR+TIMEM) = TIME
               LTIME = TIME
               EDCORI(IPTR+1) = MAXREC + 1
               IPTR = IPTR + 1
               EDCORI(JPTR+1) = SOURC
               JPTR = JPTR + 1
               END IF
C                                       basic pointers
            EDCORI(LPTR+3) = CALTYP
            EDCORI(LPTR+2) = IROW
            EDCORI(LPTR) = IAN
            EDCORI(LPTR+1) = TIMEM
            MAXREC = MAXREC + 1
            LPTR = LPTR + DIMREC
            MSAMPS(IAN) = MSAMPS(IAN) + 1
 60         CONTINUE
         IERR = 0
C                                       finish the times
 70      TIMES(EDIPTR+TIMEM+1) = TIMES(EDIPTR+TIMEM) + 2*DTIME
         TIMES(EDIPTR+TIMEM+2) = TIMES(EDIPTR+TIMEM) + 4*DTIME
         TIMES(EDIPTR+TIMEM+3) = TIMES(EDIPTR+TIMEM) + 6*DTIME
         TIMES(EDIPTR+TIMEM+4) = TIMES(EDIPTR+TIMEM) + 8*DTIME
         EDCORI(IPTR+1) = MAXREC + 1
         EDCORI(IPTR+2) = MAXREC + 1
         EDCORI(IPTR+3) = MAXREC + 1
         EDCORI(IPTR+4) = MAXREC + 1
         EDCORI(IPTR+5) = MAXREC + 1
         TIMEM = TIMEM + 4
         ITIM = TIMEM - 8 - ITIM
         WRITE (MSGTXT,1070) ITIM, TIMEM
         CALL MSGWRT (3)
         END IF
C                                       All calls:
C                                       if allin and in the middle,
C                                       there is nothing to do
      IF ((.NOT.ALLIN) .OR. (PIFNOW.LT.0) .OR. (IFNOW.LT.0)) THEN
         IF (ALLIN) THEN
            LF1 = BIF
            LF2 = EIF
            PLF1 = LF1
            PLF2 = LF2
         ELSE
            LF1 = IFNOW
            LF2 = IFNOW
            PLF1 = PIFNOW
            PLF2 = PIFNOW
            END IF
         LPTR = DPTR
         DO 190 IREC = 1,MAXREC
            IROW = EDCORI(LPTR+2)
            LA = EDCORI(LPTR)
            CALTYP = EDCORI(LPTR+3)
C                                       weight = 1 for flag
            DO 150 IPOL = 1,POLMAX
               DO 140 IP = 1,3
                  ICL = 5 + IP + (IPOL - 1) * 3
                  CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *               VALUE, CDUMMY, IERR)
                  IF (IERR.GT.0) GO TO 990
                  IF (IERR.LT.0) GO TO 185
C                                       write data back to disk
                  IF ((PIFNOW.GE.0) .AND. (WASFLG.GE.0) .AND.
     *               (.NOT.UVFLAG)) THEN
                     JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF -
     *                  PLF1
                     KPTR = JPTR + IP * DIMIF
                     DO 110 LF = PLF1,PLF2
                        IF (EDCORE(JPTR+LF).LE.0.0) THEN
                           VALUE(LF) = FBLANK
                        ELSE
                           VALUE(LF) = EDCORE(KPTR+LF)
                           END IF
 110                    CONTINUE
                     CALL TABDPT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *                  VALUE, CDUMMY, IERR)
                     IF (IERR.NE.0) GO TO 990
                     END IF
C                                       load new data to core
                  IF (IFNOW.GE.0) THEN
                     JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF -
     *                  LF1
                     KPTR = JPTR + IP*DIMIF
                     DO 120 LF = LF1,LF2
                        IF (VALUE(LF).EQ.FBLANK) THEN
                           EDCORE(JPTR+LF) = 0.0
                           EDCORE(KPTR+LF) = VALUE(LF)
                           EDCORE(KPTR+LF+4*DIMIF) = FBLANK
                        ELSE
                           EDCORE(JPTR+LF) = 1.0
                           EDCORE(KPTR+LF) = VALUE(LF)
                           EDCORE(KPTR+LF+4*DIMIF) = 0.0
                           END IF
 120                    CONTINUE
                     END IF
 140              CONTINUE
C                                       Psum/Pdif
C                                       load new data to core
               IF (IFNOW.GE.0) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF -
     *               LF1
                  KPTR = JPTR + 4*DIMIF
                  DO 145 LF = LF1,LF2
                     VS = EDCORE(JPTR+LF+2*DIMIF)
                     VD = EDCORE(JPTR+LF+DIMIF)
                     IF (CALTYP.EQ.1) THEN
                        TC = TCAL(IPOL+2,LF,LA)
                     ELSE
                        TC = TCAL(IPOL,LF,LA)
                        END IF
                     IF ((VS.EQ.FBLANK) .OR. (VD.EQ.FBLANK) .OR.
     *                  (VD.EQ.0.0) .OR. (EDCORE(JPTR+LF).LE.0.0)) THEN
                        EDCORE(JPTR+LF) = 0.0
                        EDCORE(KPTR+LF) = FBLANK
                        EDCORE(KPTR+LF+4*DIMIF) = FBLANK
                     ELSE IF (TC.NE.FBLANK) THEN
                        EDCORE(KPTR+LF) = VS / VD / 2.0 * TC
                        EDCORE(KPTR+LF+4*DIMIF) = 0.0
                     ELSE
                        EDCORE(KPTR+LF) = FBLANK
                        EDCORE(KPTR+LF+4*DIMIF) = FBLANK
                        END IF
 145                 CONTINUE
                  END IF
 150           CONTINUE
 185        LPTR = LPTR + DIMREC
 190        CONTINUE
         WASFLG = -1
         IF (IFNOW.GE.0) THEN
            CALL EDFGAP (UVMAST, IERR)
            IF (IERR.EQ.0) FLAGED = .TRUE.
            END IF
         END IF
C                                       redo flags
      IF ((.NOT.ALLIN) .AND. (IFNOW.GT.0) .AND. (IFWASN(IFNOW))) THEN
         CALL COPY (5, GRSEL(4), GRNONE)
         CALL FILL (5, 0, GRSEL(4))
         CALL EDFCDO (IERR)
         CALL COPY (5, GRNONE, GRSEL(4))
         END IF
      GO TO 999
C                                       write error
 990  WRITE (MSGTXT,1990) IREC, IROW, COLLAB(ICL)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Write data back to disk: change count =',I8)
 1070 FORMAT ('Found',I6,' times, stored on a time axis of length',I6)
 1990 FORMAT ('EDGTSY: ERROR READING REC',I5,' ROW',I6,' COL ',A)
 4000 FORMAT ('Allocate ',I12,' words at',I18)
      END
      SUBROUTINE EDGTUV (IERR)
C-----------------------------------------------------------------------
C Gets UV data from OBJECT for specified IF(s), antennas(s) C Inputs:
C OBJECT C*(*) Table object name C Output: C IERR I Error code: > 0 =>
C quit
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DDCH.INC'
      LONGINT   LPTR
      INTEGER   I, IAN, DIM(7), ITIM, ICH, LTIM, INDXS, IIF, LF1, DUMMY,
     *   LF2, IREC, SOURC, GRNONE(5), JJP, NS, INDXT, INDXB, INDXA1,
     *   INDXA2, IA1, IA2, TOTS, MAXS, IINCS, IINCF, IICIF, IP, TYPE,
     *   TOTREC, NSAMPS(MAXANT,MAXANT)
      REAL      VIS(3,MAXCIF), RP(20), TIME, LTIME, HTIM, WT, DR, DI,
     *   LTIMES(30000)
      CHARACTER PROBLM*32, CDUMMY*1, UVTYPE*2
      SAVE VIS
      COMMON /EDISCR/ LTIMES
C-----------------------------------------------------------------------
      IF (IFNOW.LT.0) THEN
         MSGTXT = 'EDITUV CALLED WITH IFNOW < 0 - NOT ALLOWED'
         IERR = 6
         CALL MSGWRT (7)
         GO TO 999
         END IF
      IF ((ANTNOW.EQ.0) .OR. (ANTNOW.LT.-1)) THEN
         MSGTXT = 'EDITUV CALLED WITH ANTNOW 0 - NOT ALLOWED'
         IERR = 6
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       First time
      IF (MAXREC.LE.0) THEN
         WASACR = .FALSE.
         WASXCR = .FALSE.
         IF (DTIME.LE.0.0) DTIME = 1.0 / 6000.0
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0 * DTIME)
         DTIME = DTIME / (24 * 60.)
         GTIME = GTIME / (24 * 60.)
         HTIM = DTIME/2.0
C                                       Get column pointers
         DIMPRM = 2
         DIMIF = EIF - BIF + 1
         DIMDAT = 5
         IF (UVDAT2.NE.' ') DIMDAT = 9
         DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
C                                       read through file to count data
C                                       and to study times.
         TOTREC = 0
         MAXREC = 0
         TIMEM = 0
         ITIM = 0
         CALL FILL (MAXANT, 0, MSAMPS)
         NS = MAXANT * MAXANT
         CALL FILL (NS, 0, NSAMPS)
C                                       open the uv data
         PROBLM = UVMAST
         CALL OUVOPN (UVMAST, 'READ', IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Check data type
         CALL UVDGET (UVMAST, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       get pointers
         CALL UVDFND (UVMAST, 1, 'TIME', INDXT, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER TIME'
            GO TO 980
            END IF
         IF (UVTYPE(:1).EQ.'S') THEN
            CALL UVDFND (UVMAST, 1, 'BEAM', INDXB, IERR)
         ELSE
            CALL UVDFND (UVMAST, 1, 'BASELINE', INDXB, IERR)
            IF (IERR.NE.0) THEN
               CALL UVDFND (UVMAST, 1, 'ANTENNA1', INDXA1, IERR)
               IF (IERR.EQ.0) CALL UVDFND (UVMAST, 1, 'ANTENNA2',
     *            INDXA2, IERR)
               END IF
            END IF
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER BASELINE'
            GO TO 980
            END IF
         MSGTXT = 'Read the data to list times'
         CALL MSGWRT (2)
C                                       list times
C                                       read loop
 20      CALL UVREAD (UVMAST, RP, VIS, IERR)
         IF (IERR.GT.0) GO TO 990
         IF (IERR.LT.0) GO TO 40
            TIME = RP(INDXT)
            IF (TIMEM.EQ.0) THEN
               TIMEM = 4
               LTIME = TIME
               LTIMES(TIMEM) = TIME + HTIM
               LTIMES(3) = LTIMES(4) - 2*DTIME
               LTIMES(2) = LTIMES(4) - 4*DTIME
               LTIMES(1) = LTIMES(4) - 6*DTIME
C                                       count up samples in last
C                                       integration
            ELSE IF (TIME-LTIME.GT.DTIME) THEN
               TIMEM = TIMEM + 1
               IF (TIME-LTIME.GT.2*GTIME) THEN
                  ITIM = ITIM + 2
                  TIMEM = TIMEM + 2
                  LTIMES(TIMEM-1) = (2.*TIME + LTIME)/3.0 + HTIM
                  LTIMES(TIMEM-2) = (TIME + 2.*LTIME)/3.0 + HTIM
               ELSE IF (TIME-LTIME.GT.GTIME) THEN
                  ITIM = ITIM + 1
                  TIMEM = TIMEM + 1
                  LTIMES(TIMEM-1) = (TIME + LTIME) / 2.0 + HTIM
                  END IF
               LTIMES(TIMEM) = TIME + HTIM
               LTIME = TIME
               IF (MAXREC.GT.0) THEN
                  MAXREC = 0
                  DO 30 IA1 = 1,MAXANT
                     DO 25 IA2 = 1,MAXANT
                        IF (NSAMPS(IA1,IA2).GT.0) THEN
                           MSAMPS(IA1) = MSAMPS(IA1) + 1
                           MSAMPS(IA2) = MSAMPS(IA2) + 1
                           NSAMPS(IA1,IA2) = 0
                           END IF
 25                     CONTINUE
 30                  CONTINUE
                  END IF
               END IF
            IF (INDXB.GT.0) THEN
               IAN = RP(INDXB) + 0.1
               CALL EDANTS (IAN, IA1, IA2)
            ELSE
               IA1 = RP(INDXA1) + 0.1
               IA2 = RP(INDXA2) + 0.1
               IAN = 256 * IA1 + IA2
               END IF
            NSAMPS(IA1,IA2) = NSAMPS(IA1,IA2) + 1
            MAXREC = MAXREC + 1
            TOTREC = TOTREC + 1
            GO TO 20
C                                       finish the times
 40      LTIMES(TIMEM+1) = LTIMES(TIMEM) + 2*DTIME
         LTIMES(TIMEM+2) = LTIMES(TIMEM) + 4*DTIME
         LTIMES(TIMEM+3) = LTIMES(TIMEM) + 6*DTIME
         LTIMES(TIMEM+4) = LTIMES(TIMEM) + 8*DTIME
         LTIMES(TIMEM+5) = LTIMES(TIMEM) + 10*DTIME
         TIMEM = TIMEM + 5
C                                       close the uv data
         CALL OUVCLO (UVMAST, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       how big is this data set
         MAXS = 0
         TOTS = 0
         ANTMLX = 0
         ANTMAX = 0
         DO 45 I = 1,MAXANT
            IF (MSAMPS(I).GT.0) THEN
               ANTMAX = I
               ANTMLX = ANTMLX + 1
               ANTEN(ANTMLX) = I
               MAXS = MAX (MAXS, MSAMPS(I))
               TOTS = TOTS + MSAMPS(I)
               END IF
 45         CONTINUE
         TOTS = TOTS / 2
         IF (MAXS.LE.1) THEN
            MSGTXT = 'NO DATA FOUND - CHECK DATA SELECTION'
            IERR = 8
            GO TO 980
            END IF
C                                       set the display antennas
         IF (ANTNOW.EQ.-1) ANTNOW = ANTEN(1)
C                                       TY no use for SC area
         SCMAXS = 100
C                                       How big is the problem
C                                       Try all
         MAXREC = TOTS
         I = TOTREC * DIMREC + 4 * TIMEM + 401 + SCMAXS
         EDSIZE = I + 4096
         EDSIZE = (EDSIZE - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'EDGTUV', EDSIZE, EDCORE, EDIPTR, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'UNABLE TO ALLOCATE NEEDED MEMORY'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         EDSIZE = EDSIZE * 1024
         WRITE (MSGTXT,4000) EDSIZE, EDIPTR
         CALL MSGWRT (9)
4000  FORMAT ('Allocate ',I9,' words at',I18)
         ALLIN = I.LT.EDSIZE
         ONEANT = .NOT.ALLIN
C                                       Try 1 IF, all BL
         IF (.NOT.ALLIN) THEN
            DIMIF = 1
            DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
            I = MAXREC * DIMREC + 3 * (TIMEM + 100) + SCMAXS
            ONEANT = .FALSE.
C                                       Try 1 ant, all IF
            IF (I.GT.EDSIZE) THEN
               ONEANT = .TRUE.
               ALLIN = .TRUE.
               MAXREC = MAXS
               DIMIF = EIF - BIF + 1
               DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
               I = MAXREC * DIMREC + 3 * (TIMEM + 100) + SCMAXS
C                                       Try 1 ant, 1 IF
               IF (I.GT.EDSIZE) THEN
                  ALLIN = .FALSE.
                  DIMIF = 1
                  DIMREC = DIMPRM + DIMIF * DIMDAT * POLMAX
                  I = MAXREC * DIMREC + 3 * (TIMEM + 100) + SCMAXS
C                                       failure
                  IF (I.GT.EDSIZE) THEN
                     MSGTXT = 'ONE IF STILL TOO BIG FOR MEMORY: ' //
     *                  'USE TIMERANG OR ANTENNAS'
                     CALL MSGWRT (8)
                     IERR = 8
                     GO TO 999
                     END IF
                  END IF
               END IF
            END IF
         IF (ONEANT) THEN
            IF (ALLIN) THEN
               MSGTXT = 'Will Do ALL IFs to ONE antenna in memory at'
     *            // ' a time'
            ELSE
               MSGTXT = 'Will do ONE IF to ONE antenna in memory at'
     *            // ' a time'
               END IF
         ELSE
            IF (ALLIN) THEN
               MSGTXT = 'All data will reside in memory'
            ELSE
               MSGTXT = 'Will do only ONE IF to ALL baselines in'
     *            // ' memory at a time'
               END IF
            END IF
         CALL MSGWRT (3)
         ITIM = TIMEM - 8 - ITIM
         WRITE (MSGTXT,1070) ITIM, TIMEM
         CALL MSGWRT (3)
C                                       now fill in times, ants, ..
         CALL RCOPY (TIMEM, LTIMES, TIMES(EDIPTR+1))
         SUPTR = TIMEM + 101 + EDIPTR
         PPTR = SUPTR + TIMEM + 100
         SPTR = PPTR + TIMEM + 100
         DPTR = SPTR + SCMAXS
         END IF
C                                       All calls:
C                                       if allin and in the middle,
C                                       there is nothing to do
      IF ((.NOT.ALLIN) .OR. (ONEANT) .OR. (PIFNOW.LT.0) .OR.
     *   (PANTNW.LE.0)) THEN
         IF (ALLIN) THEN
            LF1 = BIF
            LF2 = EIF
         ELSE
            LF1 = IFNOW
            LF2 = IFNOW
            END IF
C                                       open the uv data
         PROBLM = UVMAST
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (UVMAST, 'CALEDIT.BIF', OOAINT, DIM, LF1, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVMAST, 'CALEDIT.EIF', OOAINT, DIM, LF2, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVOPN (UVMAST, 'READ', IERR)
         IF (IERR.NE.0) GO TO 990
         SOURC = SUNUMB
         MSGTXT = 'Read the data into memory'
         CALL MSGWRT (2)
C                                       get pointers
         CALL UVDPNT (UVMAST, DUMMY, DUMMY, DUMMY, INDXT, INDXB, INDXS,
     *      DUMMY, INDXA1, INDXA2, DUMMY, DUMMY, DUMMY, DUMMY, DUMMY,
     *      DUMMY, DUMMY, IINCS, IINCF, IICIF, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Convert increments to work with
C                                       VIS(3,*)
         IINCS = IINCS / 3
         IINCF = IINCF / 3
         IICIF = IICIF / 3
         ITIM = 2
         LTIM = 0
         MAXREC = 0
C                                       zero memory
         I = EDSIZE - TIMEM
         CALL RFILL (I, 0.0, EDCORE(EDIPTR+TIMEM+1))
C                                       list times
C                                       read loop
 100     CALL UVREAD (UVMAST, RP, VIS, IERR)
         IF (IERR.GT.0) GO TO 990
         IF (IERR.LT.0) GO TO 150
            TIME = RP(INDXT)
            IF (INDXS.GT.0) SOURC = RP(INDXS) + 0.1
            IF (INDXB.GT.0) THEN
               IAN = RP(INDXB) + 0.1
               CALL EDANTS (IAN, IA1, IA2)
            ELSE
               IA1 = RP(INDXA1) + 0.1
               IA2 = RP(INDXA2) + 0.1
               IAN = 256 * IA1 + IA2
               END IF
            IF ((ONEANT) .AND. (IA1.NE.ANTNOW) .AND. (IA2.NE.ANTNOW))
     *         GO TO 100
C                                       find time to which this applies
            DO 105 I = ITIM,TIMEM-1
               IF ((TIME.GE.(TIMES(EDIPTR+I)+TIMES(EDIPTR+I-1))/2.0)
     *            .AND. (TIME.LT.
     *            (TIMES(EDIPTR+I)+TIMES(EDIPTR+I+1))/2.0)) THEN
                  ITIM = I
                  GO TO 110
                  END IF
 105           CONTINUE
            GO TO 100
C                                       time step
 110        IF (ITIM.NE.LTIM) THEN
               MAXREC = MAXREC + 1
C                                       record and source pointers
               DO 115 I = LTIM+1,ITIM
                  EDCORI(PPTR+I-1) = MAXREC
                  EDCORI(SUPTR+I-1) = SOURC
 115              CONTINUE
               IREC = MAXREC
               LTIM = ITIM
C                                       in current time
            ELSE
               NS = EDCORI(PPTR+ITIM-1)
               DO 120 IREC = NS,MAXREC
                  LPTR = DPTR + (IREC-1) * DIMREC
                  IF (IAN.EQ.EDCORI(LPTR)) GO TO 125
 120              CONTINUE
               MAXREC = MAXREC + 1
               IREC = MAXREC
               END IF
C                                       add it in
            LPTR = DPTR + (IREC-1) * DIMREC
            EDCORI(LPTR) = IAN
            EDCORI(LPTR+1) = ITIM
 125        DO 140 IP = 1,POLMAX
               DO 135 IIF = LF1,LF2
                  LPTR = DPTR + (IREC-1) * DIMREC + 2 +
     *               (IP-1) * DIMIF * DIMDAT + IIF - LF1
                  DO 130 ICH = BCHAN,ECHAN
                     JJP = 1 + (IIF-LF1) * IICIF + (ICH-BCHAN) * IINCF
     *                  + (IP - 1) * IINCS
                     WT = VIS(3,JJP) * WTSCAL
                     IF (WT.GT.0.0) THEN
                        EDCORE(LPTR) = EDCORE(LPTR) + WT
                        EDCORE(LPTR+DIMIF) = EDCORE(LPTR+DIMIF) +
     *                     WT * VIS(1,JJP)
                        IF (IA1.NE.IA2) THEN
                           EDCORE(LPTR+2*DIMIF) = EDCORE(LPTR+2*DIMIF) +
     *                        WT * VIS(2,JJP)
                           WASXCR = .TRUE.
                        ELSE
                           WASACR = .TRUE.
                           END IF
                        END IF
 130                 CONTINUE
 135              CONTINUE
 140           CONTINUE
            GO TO 100
C                                       record and source pointers end
 150     DO 155 I = ITIM+1,TIMEM
            EDCORI(PPTR+I-1) = MAXREC + 1
            EDCORI(SUPTR+I-1) = SOURC
 155        CONTINUE
C                                       close the uv data
         CALL OUVCLO (UVMAST, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       second data set
         IF (UVDAT2.NE.' ') THEN
C                                       open the uv data
            PROBLM = UVDAT2
            LF1 = LF1 - BIF + BIF2
            LF2 = LF2 - BIF + BIF2
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (UVDAT2, 'CALEDIT.BIF', OOAINT, DIM, LF1, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL OPUT (UVDAT2, 'CALEDIT.EIF', OOAINT, DIM, LF2, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL OUVOPN (UVDAT2, 'READ', IERR)
            IF (IERR.NE.0) GO TO 990
C                                       get pointers
            CALL UVDPNT (UVDAT2, DUMMY, DUMMY, DUMMY, INDXT, INDXB,
     *         INDXS, DUMMY, INDXA1, INDXA2, DUMMY, DUMMY, DUMMY, DUMMY,
     *         DUMMY, DUMMY, DUMMY, IINCS, IINCF, IICIF, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Convert increments to work with
C                                       VIS(3,*)
            IINCS = IINCS / 3
            IINCF = IINCF / 3
            IICIF = IICIF / 3
            ITIM = 1
            LTIM = 0
            MSGTXT = 'Read the 2nd uv file data into memory'
            CALL MSGWRT (2)
C                                       list times
C                                       read loop
 200        CALL UVREAD (UVDAT2, RP, VIS, IERR)
            IF (IERR.GT.0) GO TO 990
            IF (IERR.LT.0) GO TO 250
               TIME = RP(INDXT)
               IF (INDXB.GT.0) THEN
                  IAN = RP(INDXB) + 0.1
                  CALL EDANTS (IAN, IA1, IA2)
               ELSE
                  IA1 = RP(INDXA1) + 0.1
                  IA2 = RP(INDXA2) + 0.1
                  IAN = 256 * IA1 + IA2
                  END IF
               IF ((ONEANT) .AND. (IA1.NE.ANTNOW) .AND. (IA2.NE.ANTNOW))
     *            GO TO 200
C                                       find time to which this applies
               DO 205 ITIM = 2,TIMEM-1
                  IF ((TIME.GE.(TIMES(EDIPTR+ITIM)+
     *               TIMES(EDIPTR+ITIM-1))/2.0) .AND. (TIME.LT.
     *               (TIMES(EDIPTR+ITIM)+TIMES(EDIPTR+ITIM+1))/2.0))
     *               GO TO 215
 205              CONTINUE
               LTIM = LTIM + 1
               GO TO 200
C                                       find match
 215           NS = EDCORI(PPTR+ITIM-1)
               ICH = EDCORI(PPTR+ITIM) - 1
               DO 220 IREC = NS,ICH
                  LPTR = DPTR + (IREC-1) * DIMREC
                  IF (IAN.EQ.EDCORI(LPTR)) GO TO 225
 220              CONTINUE
               LTIM = LTIM + 1
               GO TO 200
C                                       add it in
 225           DO 240 IP = 1,POLMAX
                  DO 235 IIF = LF1,LF2
                     LPTR = DPTR + (IREC-1) * DIMREC + 2 + 5 * DIMIF +
     *                  (IP-1) * DIMIF * DIMDAT + IIF - LF1
                     DO 230 ICH = BCHAN,ECHAN
                        JJP = 1 + (IIF - LF1) * IICIF +
     *                     (ICH - BCHAN) * IINCF + (IP - 1) * IINCS
                        WT = VIS(3,JJP) * WTSCAL
                        IF (WT.GT.0.0) THEN
                           EDCORE(LPTR+2*DIMIF) = EDCORE(LPTR+2*DIMIF) +
     *                        WT
                           EDCORE(LPTR) = EDCORE(LPTR) +
     *                        WT * VIS(1,JJP)
                           IF (IA1.NE.IA2) THEN
                              EDCORE(LPTR+DIMIF) = EDCORE(LPTR+DIMIF) +
     *                           WT * VIS(2,JJP)
                              WASXCR = .TRUE.
                           ELSE
                              WASACR = .TRUE.
                              END IF
                           END IF
 230                    CONTINUE
 235                 CONTINUE
 240              CONTINUE
               GO TO 200
C                                       close the uv data
 250        CALL OUVCLO (UVDAT2, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (LTIM.GT.0) THEN
               WRITE (MSGTXT,1250) LTIM
               CALL MSGWRT (6)
               END IF
            END IF
C                                       average, -> amp & phase
         DO 330 IREC = 1,MAXREC
            LPTR = DPTR + (IREC-1) * DIMREC
            IAN = EDCORI(LPTR)
            CALL EDANTS (IAN, IA1, IA2)
            DO 320 IP = 1,POLMAX
               DO 310 IIF = LF1,LF2
                  LPTR = DPTR + (IREC-1) * DIMREC + 2 +
     *               (IP-1) * DIMIF * DIMDAT + IIF - LF1
                  WT = EDCORE(LPTR)
                  IF (WT.LE.0.0) THEN
                     EDCORE(LPTR) = 0.0
                     EDCORE(LPTR+DIMIF) = FBLANK
                     EDCORE(LPTR+2*DIMIF) = FBLANK
                  ELSE
                     DR = EDCORE(LPTR+DIMIF) / WT
                     IF (IA1.EQ.IA2) THEN
                        EDCORE(LPTR+DIMIF) = DR
                        EDCORE(LPTR+2*DIMIF) = 0.0
                     ELSE
                        DI = EDCORE(LPTR+2*DIMIF) / WT
                        EDCORE(LPTR+DIMIF) = SQRT (DR*DR + DI*DI)
                        IF ((DR.EQ.0) .AND. (DI.EQ.0)) THEN
                           EDCORE(LPTR+2*DIMIF) = FBLANK
                        ELSE
                           EDCORE(LPTR+2*DIMIF) = ATAN2 (DI,DR) / DG2RAD
                           END IF
                        END IF
                     END IF
                  IF (UVDAT2.NE.' ') THEN
                     LPTR = LPTR + 5 * DIMIF
                     WT = EDCORE(LPTR+2*DIMIF)
                     IF (WT.LE.0.0) THEN
                        EDCORE(LPTR) = FBLANK
                        EDCORE(LPTR+DIMIF) = FBLANK
                     ELSE
                        DR = EDCORE(LPTR) / WT
                        IF (IA1.EQ.IA2) THEN
                           EDCORE(LPTR) = DR
                           EDCORE(LPTR+DIMIF) = 0.0
                        ELSE
                           DI = EDCORE(LPTR+DIMIF) / WT
                           EDCORE(LPTR) = SQRT (DR*DR + DI*DI)
                           IF ((DR.EQ.0) .AND. (DI.EQ.0)) THEN
                              EDCORE(LPTR+DIMIF) = FBLANK
                           ELSE
                              EDCORE(LPTR+DIMIF) = ATAN2 (DI,DR)/DG2RAD
                              END IF
                           END IF
                        END IF
                     EDCORE(LPTR+2*DIMIF) = 0.0
                     END IF
 310              CONTINUE
 320           CONTINUE
 330        CONTINUE
         FLAGED = .TRUE.
         END IF
      WASFLG = -1
C                                       redo flags
      IF ((.NOT.ALLIN) .AND. ((IFNOW.GT.0) .AND. (IFWASN(IFNOW))) .OR.
     *   (ONEANT)) THEN
         CALL COPY (5, GRSEL(4), GRNONE)
         CALL FILL (5, 0, GRSEL(4))
         CALL EDFCDO (IERR)
         CALL COPY (5, GRNONE, GRSEL(4))
         END IF
      GO TO 999
C
 980  CALL MSGWRT (7)
C
 990  MSGTXT = 'PROBLEM WITH ' // PROBLM
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('Found',I6,' times, stored on a time axis of length',I6)
 1250 FORMAT ('Warning:',I6,' samples in 2nd data set did not match',
     *   ' 1st')
      END
      SUBROUTINE EDIFSY
C-----------------------------------------------------------------------
C   Computes the difference between the current value and a running
C   average value for Tsys and stores that in the phase location.
C   Inputs and outputs from common only
C-----------------------------------------------------------------------
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      LONGINT   LPTR, JPTR
      INTEGER   IT0, IT1, IT2, I, I1, IA, NDIFS(2,MAXIF,MAXANT), J, IS,
     *   IP, IIF, NSUMS(2,MAXIF,MAXANT), NGANS(2,MAXIF,MAXANT),
     *   NRATS(2,MAXIF,MAXANT)
      REAL      PDIFS(2,MAXIF,MAXANT), PSUMS(2,MAXIF,MAXANT), THALF, T,
     *   PGANS(2,MAXIF,MAXANT), PRATS(2,MAXIF,MAXANT)
C-----------------------------------------------------------------------
      SMTIME(1) = MAX (0.2, SMTIME(1))
      THALF = SMTIME(1) / (24.0 * 3600.0)
C                                       loop over all times
      I1 = 1
      DO 100 IT0 = 1,TIMEM
C                                       find range of times that apply
         IS = EDCORI(SUPTR+IT0-1)
         IT1 = IT0
         T = TIMES(EDIPTR+IT0) - THALF
         DO 10 I = 1,TIMEM
            J = IT0 - I
            IF ((J.LT.1) .OR. (EDCORI(SUPTR+J-1).NE.IS) .OR.
     *         (TIMES(EDIPTR+J).LT.T)) GO TO 15
               IT1 = J
 10         CONTINUE
 15      T = TIMES(EDIPTR+IT0) + THALF
         IT2 = IT0
         DO 20 I = 1,TIMEM
            J = IT0 + I
            IF ((J.GT.TIMEM) .OR. (EDCORI(SUPTR+J-1).NE.IS) .OR.
     *         (TIMES(EDIPTR+J).GT.T)) GO TO 25
               IT2 = J
 20         CONTINUE
C                                       sum up scan
 25      DO 40 IA = 1,ANTMAX
            DO 35 IIF = 1,IFMAX
               DO 30 IP = 1,POLMAX
                  PDIFS(IP,IIF,IA) = 0.0
                  PSUMS(IP,IIF,IA) = 0.0
                  PGANS(IP,IIF,IA) = 0.0
                  PRATS(IP,IIF,IA) = 0.0
                  NDIFS(IP,IIF,IA) = 0
                  NSUMS(IP,IIF,IA) = 0
                  NGANS(IP,IIF,IA) = 0
                  NRATS(IP,IIF,IA) = 0
 30               CONTINUE
 35            CONTINUE
 40         CONTINUE
         LPTR = DPTR + (I1 - 1) * DIMREC
         DO 60 I = I1,MAXREC
            IF (IT1.GT.EDCORI(LPTR+1)) THEN
               I1 = I
            ELSE IF (IT2.LT.EDCORI(LPTR+1)) THEN
               GO TO 65
            ELSE
               IA = EDCORI(LPTR)
               DO 50 IP = 1,POLMAX
                  JPTR = DIMPRM + (IP - 1) * DIMIF * DIMDAT + LPTR
                  DO 45 IIF = 1,DIMIF
                     IF (EDCORE(JPTR).GT.0.0) THEN
                        IF (EDCORE(JPTR+DIMIF).NE.FBLANK) THEN
                           NDIFS(IP,IIF,IA) = NDIFS(IP,IIF,IA) + 1
                           PDIFS(IP,IIF,IA) = PDIFS(IP,IIF,IA) +
     *                        EDCORE(JPTR+DIMIF)
                           END IF
                        IF (EDCORE(JPTR+2*DIMIF).NE.FBLANK) THEN
                           NSUMS(IP,IIF,IA) = NSUMS(IP,IIF,IA) + 1
                           PSUMS(IP,IIF,IA) = PSUMS(IP,IIF,IA) +
     *                        EDCORE(JPTR+2*DIMIF)
                           END IF
                        IF (EDCORE(JPTR+3*DIMIF).NE.FBLANK) THEN
                           NGANS(IP,IIF,IA) = NGANS(IP,IIF,IA) + 1
                           PGANS(IP,IIF,IA) = PGANS(IP,IIF,IA) +
     *                        EDCORE(JPTR+3*DIMIF)
                           END IF
                        IF (EDCORE(JPTR+4*DIMIF).NE.FBLANK) THEN
                           NRATS(IP,IIF,IA) = NRATS(IP,IIF,IA) + 1
                           PRATS(IP,IIF,IA) = PRATS(IP,IIF,IA) +
     *                        EDCORE(JPTR+4*DIMIF)
                           END IF
                        END IF
                     JPTR = JPTR + 1
 45                  CONTINUE
 50               CONTINUE
               END IF
            LPTR = LPTR + DIMREC
 60         CONTINUE
C                                       store result
 65      LPTR = DPTR + (I1 - 1) * DIMREC
         DO 80 I = I1,MAXREC
            IF (IT0.LT.EDCORI(LPTR+1)) THEN
               GO TO 100
            ELSE IF (IT0.EQ.EDCORI(LPTR+1)) THEN
               IA = EDCORI(LPTR)
               DO 75 IP = 1,POLMAX
                  JPTR = DIMPRM + (IP - 1) * DIMIF * DIMDAT + LPTR
                  DO 70 IIF = 1,DIMIF
                     IF ((EDCORE(JPTR).GT.0.0) .AND.
     *                  (EDCORE(JPTR+DIMIF).NE.FBLANK) .AND.
     *                  (NDIFS(IP,IIF,IA).GT.0) .AND.
     *                  (PDIFS(IP,IIF,IA).GT.0.0)) THEN
                        EDCORE(JPTR+5*DIMIF) = EDCORE(JPTR+DIMIF) *
     *                     NDIFS(IP,IIF,IA) / PDIFS(IP,IIF,IA)  - 1.0
                     ELSE
                        EDCORE(JPTR+5*DIMIF) = FBLANK
                        END IF
                     IF ((EDCORE(JPTR).GT.0.0) .AND.
     *                  (EDCORE(JPTR+2*DIMIF).NE.FBLANK) .AND.
     *                  (NSUMS(IP,IIF,IA).GT.0) .AND.
     *                  (PSUMS(IP,IIF,IA).GT.0.0)) THEN
                        EDCORE(JPTR+6*DIMIF) = EDCORE(JPTR+2*DIMIF) *
     *                     NSUMS(IP,IIF,IA) / PSUMS(IP,IIF,IA)  - 1.0
                     ELSE
                        EDCORE(JPTR+6*DIMIF) = FBLANK
                        END IF
                     IF ((EDCORE(JPTR).GT.0.0) .AND.
     *                  (EDCORE(JPTR+3*DIMIF).NE.FBLANK) .AND.
     *                  (NGANS(IP,IIF,IA).GT.0) .AND.
     *                  (PGANS(IP,IIF,IA).GT.0.0)) THEN
                        EDCORE(JPTR+7*DIMIF) = EDCORE(JPTR+3*DIMIF) *
     *                     NGANS(IP,IIF,IA) / PGANS(IP,IIF,IA)  - 1.0
                     ELSE
                        EDCORE(JPTR+7*DIMIF) = FBLANK
                        END IF
                     IF ((EDCORE(JPTR).GT.0.0) .AND.
     *                  (EDCORE(JPTR+4*DIMIF).NE.FBLANK) .AND.
     *                  (NRATS(IP,IIF,IA).GT.0) .AND.
     *                  (PRATS(IP,IIF,IA).GT.0.0)) THEN
                        EDCORE(JPTR+8*DIMIF) = EDCORE(JPTR+4*DIMIF) *
     *                     NRATS(IP,IIF,IA) / PRATS(IP,IIF,IA)  - 1.0
                     ELSE
                        EDCORE(JPTR+8*DIMIF) = FBLANK
                        END IF
                     JPTR = JPTR + 1
 70                  CONTINUE
 75               CONTINUE
               END IF
            LPTR = LPTR + DIMREC
 80         CONTINUE
 100     CONTINUE
      FLAGED = .FALSE.
C
 999  RETURN
      END
      SUBROUTINE EDIFTY
C-----------------------------------------------------------------------
C   Computes the difference between the current value and a running
C   average value for Tsys and stores that in the phase location.
C   Inputs and outputs from common only
C-----------------------------------------------------------------------
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      LONGINT   LPTR, JPTR
      INTEGER   IT0, IT1, IT2, I, I1, IA, NUMS(2,MAXIF,MAXANT), J, IS,
     *   IP, IIF
      REAL      TSYSS(2,MAXIF,MAXANT), THALF, T
C-----------------------------------------------------------------------
      SMTIME(1) = MAX (0.2, SMTIME(1))
      THALF = SMTIME(1) / (24.0 * 3600.0)
C                                       loop over all times
      I1 = 1
      DO 100 IT0 = 1,TIMEM
C                                       find range of times that apply
         IS = EDCORI(SUPTR+IT0-1)
         IT1 = IT0
         T = TIMES(EDIPTR+IT0) - THALF
         DO 10 I = 1,TIMEM
            J = IT0 - I
            IF ((J.LT.1) .OR. (EDCORI(SUPTR+J-1).NE.IS) .OR.
     *         (TIMES(EDIPTR+J).LT.T)) GO TO 15
               IT1 = J
 10         CONTINUE
 15      T = TIMES(EDIPTR+IT0) + THALF
         IT2 = IT0
         DO 20 I = 1,TIMEM
            J = IT0 + I
            IF ((J.GT.TIMEM) .OR. (EDCORI(SUPTR+J-1).NE.IS) .OR.
     *         (TIMES(EDIPTR+J).GT.T)) GO TO 25
               IT2 = J
 20         CONTINUE
C                                       sum up scan
 25      DO 40 IA = 1,ANTMAX
            DO 35 IIF = 1,IFMAX
               DO 30 IP = 1,POLMAX
                  TSYSS(IP,IIF,IA) = 0.0
                  NUMS(IP,IIF,IA) = 0
 30               CONTINUE
 35            CONTINUE
 40         CONTINUE
         LPTR = DPTR + (I1 - 1) * DIMREC
         DO 60 I = I1,MAXREC
            IF (IT1.GT.EDCORI(LPTR+1)) THEN
               I1 = I
            ELSE IF (IT2.LT.EDCORI(LPTR+1)) THEN
               GO TO 65
            ELSE
               IA = EDCORI(LPTR)
               DO 50 IP = 1,POLMAX
                  JPTR = DIMPRM + (IP - 1) * DIMIF * DIMDAT + LPTR
                  DO 45 IIF = 1,DIMIF
                     IF ((EDCORE(JPTR).GT.0.0) .AND.
     *                  (EDCORE(JPTR+DIMIF).NE.FBLANK)) THEN
                        NUMS(IP,IIF,IA) = NUMS(IP,IIF,IA) + 1
                        TSYSS(IP,IIF,IA) = TSYSS(IP,IIF,IA) +
     *                     EDCORE(JPTR+DIMIF)
                        END IF
                     JPTR = JPTR + 1
 45                  CONTINUE
 50               CONTINUE
               END IF
            LPTR = LPTR + DIMREC
 60         CONTINUE
C                                       store result
 65      LPTR = DPTR + (I1 - 1) * DIMREC
         DO 80 I = I1,MAXREC
            IF (IT0.LT.EDCORI(LPTR+1)) THEN
               GO TO 100
            ELSE IF (IT0.EQ.EDCORI(LPTR+1)) THEN
               IA = EDCORI(LPTR)
               DO 75 IP = 1,POLMAX
                  JPTR = DIMPRM + (IP - 1) * DIMIF * DIMDAT + LPTR
                  DO 70 IIF = 1,DIMIF
                     IF ((EDCORE(JPTR).GT.0.0) .AND.
     *                  (EDCORE(JPTR+DIMIF).NE.FBLANK) .AND.
     *                  (NUMS(IP,IIF,IA).GT.0) .AND.
     *                  (TSYSS(IP,IIF,IA).GT.0.0)) THEN
                        EDCORE(JPTR+2*DIMIF) = EDCORE(JPTR+DIMIF) *
     *                     NUMS(IP,IIF,IA) / TSYSS(IP,IIF,IA)  - 1.0
                     ELSE
                        EDCORE(JPTR+2*DIMIF) = FBLANK
                        END IF
                     JPTR = JPTR + 1
 70                  CONTINUE
 75               CONTINUE
               END IF
            LPTR = LPTR + DIMREC
 80         CONTINUE
 100     CONTINUE
      FLAGED = .FALSE.
C
 999  RETURN
      END
      SUBROUTINE EDIFUV
C-----------------------------------------------------------------------
C   Computes the difference between the current value and a running
C   average value for amp and phase
C   Inputs and outputs from common only
C-----------------------------------------------------------------------
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      LONGINT   LPTR, JPTR, KPTR
      INTEGER   IT0, IT1, IT2, I, I1, IA, J, IS, IP, IIF,
     *   APTR(MAXANT,MAXANT), NPTS, IL, IH, IA1, IA2, IUV, NUV, DOFF,
     *   LLPTR
      REAL      WTS(MAXANT,MAXANT), THALF, T, WT, DR, DI
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      SMTIME(1) = MAX (0.2, SMTIME(1))
      THALF = SMTIME(1) / (24.0 * 3600.0) / 2.0
      NPTS = MAXANT * MAXANT
      NUV = 1
      IF ((UVDAT2.NE.' ') .AND. (DIMDAT.EQ.9)) NUV = 2
C                                       loop over all times
      I1 = 1
      DO 100 IT0 = 1,TIMEM-1
C                                       range of records for this data
C                                       time
         IL = EDCORI(PPTR+IT0-1)
         IH = EDCORI(PPTR+IT0) - 1
         IF (IH.LT.IL) GO TO 100
C                                       find range of times that apply
         IS = EDCORI(SUPTR+IT0-1)
         IT1 = IT0
         T = TIMES(EDIPTR+IT0) - THALF
         DO 10 I = 1,TIMEM
            J = IT0 - I
            IF ((J.LT.1) .OR. (EDCORI(SUPTR+J-1).NE.IS) .OR.
     *         (TIMES(EDIPTR+J).LT.T)) GO TO 15
               IT1 = J
 10         CONTINUE
 15      T = TIMES(EDIPTR+IT0) + THALF
         IT2 = IT0
         DO 20 I = 1,TIMEM
            J = IT0 + I
            IF ((J.GT.TIMEM) .OR. (EDCORI(SUPTR+J-1).NE.IS) .OR.
     *         (TIMES(EDIPTR+J).GT.T)) GO TO 25
               IT2 = J
 20         CONTINUE
C                                       find data locations and zero
 25      CALL FILL (NPTS, 0, APTR)
         DO 35 I = IL,IH
            LPTR = DPTR + (I - 1) * DIMREC
            IA = EDCORI(LPTR)
            CALL EDANTS (IA, IA1, IA2)
            APTR(IA1,IA2) = LPTR - EDIPTR
            DO 30 IP = 1,POLMAX
               JPTR = LPTR + DIMPRM + (IP - 1) * DIMIF * DIMDAT
               JPTR = JPTR + 3 * DIMIF
               CALL RFILL (2*DIMIF, 0.0, EDCORE(JPTR))
               IF (DIMDAT.GT.5) THEN
                  JPTR = JPTR + 4 * DIMIF
                  CALL RFILL (2*DIMIF, 0.0, EDCORE(JPTR))
                  END IF
 30            CONTINUE
 35         CONTINUE
C                                       loop over pol and IF outside
         DO 90 IP = 1,POLMAX
         DO 90 IIF = 1,DIMIF
         DO 90 IUV = 1,NUV
            DOFF = 4 * (IUV - 1) * DIMIF
            CALL RFILL (NPTS, 0, WTS)
            LPTR = DPTR + (I1 - 1) * DIMREC
C                                       sum up scan
            DO 50 I = I1,MAXREC
               IF (IT1.GT.EDCORI(LPTR+1)) THEN
                  I1 = I
               ELSE IF (IT2.LT.EDCORI(LPTR+1)) THEN
                  GO TO 55
               ELSE
                  IA = EDCORI(LPTR)
                  CALL EDANTS (IA, IA1, IA2)
                  LLPTR = APTR(IA1,IA2)
                  JPTR = DIMPRM + (IP - 1) * DIMIF * DIMDAT + IIF - 1
                  WT = EDCORE(LPTR+JPTR)
                  IF ((WT.GT.0.0) .AND. (LLPTR.GT.0)) THEN
                     JPTR = JPTR + DIMIF + DOFF
                     IF (EDCORE(LPTR+JPTR).NE.FBLANK) THEN
                        WTS(IA1,IA2) = WTS(IA1,IA2) + WT
                        KPTR = LLPTR + JPTR + 2*DIMIF + EDIPTR
                        IF (IA1.EQ.IA2) THEN
                           EDCORE(KPTR) = EDCORE(KPTR) +
     *                        WT*EDCORE(LPTR+JPTR)
                        ELSE
                           EDCORE(KPTR) = EDCORE(KPTR) +
     *                        WT*EDCORE(LPTR+JPTR) *
     *                        COS (DG2RAD*EDCORE(LPTR+JPTR+DIMIF))
                           KPTR = KPTR + DIMIF
                           EDCORE(KPTR) = EDCORE(KPTR) +
     *                        WT*EDCORE(LPTR+JPTR) *
     *                        SIN (DG2RAD*EDCORE(LPTR+JPTR+DIMIF))
                           END IF
                        END IF
                     END IF
                  END IF
               LPTR = LPTR + DIMREC
 50            CONTINUE
C                                       store result
 55         DO 70 I = IL,IH
               LPTR = DPTR + (I - 1) * DIMREC
               IA = EDCORI(LPTR)
               CALL EDANTS (IA, IA1, IA2)
               JPTR = DIMPRM + (IP - 1) * DIMIF * DIMDAT + IIF - 1
               WT = WTS(IA1,IA2)
               IF ((EDCORE(LPTR+JPTR).LE.0.0) .OR. (WT.LE.0.0)) THEN
                  EDCORE(LPTR+JPTR+3*DIMIF+DOFF) = FBLANK
                  EDCORE(LPTR+JPTR+4*DIMIF+DOFF) = FBLANK
               ELSE
                  KPTR = LPTR + JPTR + DIMIF + DOFF
                  IF (IA1.EQ.IA2) THEN
                     DR = EDCORE(KPTR)
                     KPTR = KPTR + 2*DIMIF
                     EDCORE(KPTR) = DR - EDCORE(KPTR) / WT
                     EDCORE(KPTR+DIMIF) = 0.0
                  ELSE
                     DR = EDCORE(KPTR) * COS (DG2RAD*EDCORE(KPTR+DIMIF))
                     DI = EDCORE(KPTR) * SIN (DG2RAD*EDCORE(KPTR+DIMIF))
                     KPTR = KPTR + 2*DIMIF
                     DR = DR - EDCORE(KPTR) / WT
                     DI = DI - EDCORE(KPTR+DIMIF) / WT
                     EDCORE(KPTR) = SQRT (DR*DR + DI*DI)
                     IF ((DR.EQ.0.0) .AND. (DI.EQ.0.0)) THEN
                        EDCORE(KPTR+DIMIF) = 0.0
                     ELSE
                        EDCORE(KPTR+DIMIF) = ATAN2 (DI, DR) / DG2RAD
                        END IF
                     END IF
                  END IF
 70            CONTINUE
 90         CONTINUE
 100     CONTINUE
      MSGTXT = 'End computing the difference data'
      CALL MSGWRT (2)
      FLAGED = .FALSE.
C
 999  RETURN
      END
      SUBROUTINE EDMAXA (TBEDIT, ANTMAX, IERR)
C-----------------------------------------------------------------------
C   find the max antenna number the hard way
C   Inputs:
C      TBEDIT   C*(*)   Table object
C   Outputs
C      ANTMAX   I       Maximum antenna number
C      IERR     i       Error code
C-----------------------------------------------------------------------
      CHARACTER TBEDIT*(*)
      INTEGER   ANTMAX, IERR
C
      INTEGER   ANTCOL, IROW, NROW, TYPE, DIM(7), IANT
      CHARACTER COLLAB*8, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA COLLAB /'ANTENNA '/
C-----------------------------------------------------------------------
      ANTMAX = 0
C                                       number rows
      CALL OGET (TBEDIT, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'EDMAXA FAILS TO GET NROW'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       get column number
      CALL TABCOL (TBEDIT, 1, COLLAB, ANTCOL, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'EDMAXA FAILS TO GET COLUMN NUMBER'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       read the table
      DO 100 IROW = 1,NROW
         CALL TABDGT (TBEDIT, IROW, ANTCOL, TYPE, DIM, IANT, CDUMMY,
     *      IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, IROW
            CALL MSGWRT (8)
            GO TO 999
         ELSE IF (IERR.EQ.0) THEN
            IANT = MIN (IANT, MAXANT)
            ANTMAX = MAX (ANTMAX, IANT)
            END IF
 100     CONTINUE
      IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EDMAXA: ERROR',I4,' READING ROW',I8)
      END
      SUBROUTINE EDMXMN (IAN, TYPE, PIXR, IERR)
C-----------------------------------------------------------------------
C   find max and min of specified data type and antenna/baseline
C   Inputs
C      IAN      I      Antenna number
C      TYPE     I      Data type
C   Output:
C      PIXR     R(2)   Data range: min, max
C      IERR     I      error code: 1 => no valid data
C                         -1 => max = min
C-----------------------------------------------------------------------
      INTEGER   IAN, TYPE, IERR
      REAL      PIXR(2)
C
      LONGINT   LPTR, JPTR
      INTEGER   I, J, LP1, LP2, LP, LF1, LF2, LF
      REAL      V, PPIXR(2)
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'INCS:DDCH.INC'
C     DATA RSCALE  /1.E0, 1.E0, 1.E-9, 2.46791709E-13/
C-----------------------------------------------------------------------
      PIXR(1) = 1.E10
      PIXR(2) = -1.E10
      PPIXR(1) = 1.E10
      PPIXR(2) = -1.E10
      IF (POLNOW.EQ.0) THEN
         LP1 = 1
         LP2 = 2
      ELSE
         LP1 = POLNOW
         LP2 = POLNOW
         END IF
      IF (IFNOW.EQ.0) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
      DO 50 LF = LF1,LF2
         DO 40 LP = LP1,LP2
            LPTR = DPTR
            JPTR = DIMPRM + (LP-1) * DIMIF * DIMDAT
            IF (DIMIF.GT.1) JPTR = JPTR + LF - BIF
            J = TYPE * DIMIF
            DO 20 I = 1,MAXREC
               IF (IAN.EQ.EDCORI(LPTR)) THEN
                  IF (EDCORE(LPTR+JPTR).GT.0.0) THEN
                     V = EDCORE(LPTR+JPTR+J)
                     IF (V.NE.FBLANK) THEN
                        PIXR(1) = MIN (PIXR(1), V)
                        PIXR(2) = MAX (PIXR(2), V)
                        IF (TYPE.EQ.2) THEN
                           IF (V.LT.0.0) V = V + 360.
                           PPIXR(1) = MIN (PPIXR(1), V)
                           PPIXR(2) = MAX (PPIXR(2), V)
                           END IF
                        END IF
                     END IF
                  END IF
               LPTR = LPTR + DIMREC
 20            CONTINUE
            IF ((UVDAT2.NE.' ') .AND. (DIMDAT.EQ.9)) THEN
               LPTR = DPTR
               J = (TYPE + 4) * DIMIF
               DO 30 I = 1,MAXREC
                  IF (IAN.EQ.EDCORI(LPTR)) THEN
                     IF (EDCORE(LPTR+JPTR).GT.0.0) THEN
                        V = EDCORE(LPTR+JPTR+J)
                        IF (V.NE.FBLANK) THEN
                           PIXR(1) = MIN (PIXR(1), V)
                           PIXR(2) = MAX (PIXR(2), V)
                           IF (TYPE.EQ.2) THEN
                              IF (V.LT.0.0) V = V + 360.
                              PPIXR(1) = MIN (PPIXR(1), V)
                              PPIXR(2) = MAX (PPIXR(2), V)
                              END IF
                           END IF
                        END IF
                     END IF
                  LPTR = LPTR + DIMREC
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       phase - go to plus
      IF ((TYPE.EQ.2) .AND. (PPIXR(2)-PPIXR(1).LT.PIXR(2)-PIXR(1))) THEN
         DO 90 LF = LF1,LF2
            DO 80 LP = LP1,LP2
               LPTR = DPTR
               JPTR = DIMPRM + (LP-1) * DIMIF * DIMDAT
               IF (DIMIF.GT.1) JPTR = JPTR + LF - BIF
               J = TYPE * DIMIF
               DO 60 I = 1,MAXREC
                  IF (IAN.EQ.EDCORI(LPTR)) THEN
                     IF (EDCORE(LPTR+JPTR).GT.0.0) THEN
                        V = EDCORE(LPTR+JPTR+J)
                        IF (V.NE.FBLANK) THEN
                           IF (V.LT.0.0) V = V + 360.0
                           EDCORE(LPTR+JPTR+J) = V
                           END IF
                        END IF
                     END IF
                  LPTR = LPTR + DIMREC
 60               CONTINUE
               IF ((UVDAT2.NE.' ') .AND. (DIMDAT.EQ.9)) THEN
                  LPTR = DPTR
                  J = (TYPE + 4) * DIMIF
                  DO 70 I = 1,MAXREC
                     IF (IAN.EQ.EDCORI(LPTR)) THEN
                        IF (EDCORE(LPTR+JPTR).GT.0.0) THEN
                           V = EDCORE(LPTR+JPTR+J)
                           IF (V.NE.FBLANK) THEN
                              IF (V.LT.0.0) V = V + 360.0
                              EDCORE(LPTR+JPTR+J) = V
                              END IF
                           END IF
                        END IF
                     LPTR = LPTR + DIMREC
 70                  CONTINUE
                  END IF
 80            CONTINUE
 90         CONTINUE
         PIXR(1) = PPIXR(1)
         PIXR(2) = PPIXR(2)
         END IF
C                                       check range
      IERR = 0
      V = PIXR(2) - PIXR(1)
      IF ((V.GE.0.0) .AND. (V.LT.0.001*RSCALE(TYPE))) THEN
         PIXR(2) = PIXR(2) + 0.00075 * RSCALE(TYPE)
         PIXR(1) = PIXR(1) - 0.00075 * RSCALE(TYPE)
      ELSE IF (V.GT.0.0) THEN
         V = V * 0.075
         PIXR(2) = PIXR(2) + V
         PIXR(1) = PIXR(1) - V
      ELSE
         IF (PIXR(1).EQ.1.E10) IERR = 1
         IF (PIXR(2).EQ.-1.E10) IERR = 1
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDMXMT (IAN, TYPE, PIXR, IERR)
C-----------------------------------------------------------------------
C   if fixed phase plot range, try to force data into range
C   Inputs
C      IAN      I      Antenna number
C      TYPE     I      Data type
C      PIXR     R(2)   Data range: min, max
C   Output:
C      IERR     I      error code: 1 => no valid data
C                         -1 => max = min
C-----------------------------------------------------------------------
      INTEGER   IAN, TYPE, IERR
      REAL      PIXR(2)
C
      LONGINT   LPTR, JPTR
      INTEGER   I, J, LP1, LP2, LP, LF1, LF2, LF
      REAL      V
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       bad call
      IERR = 2
      IF (TYPE.NE.2) GO TO 999
      IF (PIXR(2).LE.PIXR(1)) GO TO 999
C                                       check phases
      IF (POLNOW.EQ.0) THEN
         LP1 = 1
         LP2 = 2
      ELSE
         LP1 = POLNOW
         LP2 = POLNOW
         END IF
      IF (IFNOW.EQ.0) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
      DO 50 LF = LF1,LF2
         DO 40 LP = LP1,LP2
            LPTR = DPTR
            JPTR = DIMPRM + (LP-1) * DIMIF * DIMDAT
            IF (DIMIF.GT.1) JPTR = JPTR + LF - BIF
            J = TYPE * DIMIF
            DO 20 I = 1,MAXREC
               IF (IAN.EQ.EDCORI(LPTR)) THEN
                  IF (EDCORE(LPTR+JPTR).GT.0.0) THEN
                     V = EDCORE(LPTR+JPTR+J)
                     IF (V.NE.FBLANK) THEN
                        IF (V.LT.PIXR(1)) THEN
                           V = V + 360.0
                           IF (V.LE.PIXR(2)) EDCORE(LPTR+JPTR+J) = V
                        ELSE IF (V.GT.PIXR(2)) THEN
                           V = V - 360.0
                           IF (V.GE.PIXR(1)) EDCORE(LPTR+JPTR+J) = V
                           END IF
                        END IF
                     END IF
                  END IF
               LPTR = LPTR + DIMREC
 20            CONTINUE
            IF ((UVDAT2.NE.' ') .AND. (DIMDAT.EQ.9)) THEN
               LPTR = DPTR
               J = (TYPE + 4) * DIMIF
               DO 30 I = 1,MAXREC
                  IF (IAN.EQ.EDCORI(LPTR)) THEN
                     IF (EDCORE(LPTR+JPTR).GT.0.0) THEN
                        V = EDCORE(LPTR+JPTR+J)
                        IF (V.NE.FBLANK) THEN
                           IF (V.LT.PIXR(1)) THEN
                              V = V + 360.0
                              IF (V.LE.PIXR(2)) EDCORE(LPTR+JPTR+J) = V
                           ELSE IF (V.GT.PIXR(2)) THEN
                              V = V - 360.0
                              IF (V.GE.PIXR(1)) EDCORE(LPTR+JPTR+J) = V
                              END IF
                           END IF
                        END IF
                     END IF
                  LPTR = LPTR + DIMREC
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       check range
      IERR = 0
C
 999  RETURN
      END
      LOGICAL FUNCTION EDOANT (ALL, IAN, LAN)
C-----------------------------------------------------------------------
C   determines if the current antenna is included
C   Inputs:
C      ALL      I   < 0 -> IAN and LAN must match
C                   = 0 -> all antennas/baselines match
C                   = n > 0 -> match if n = M or N in LAN = 256*M + N
C      IAN      I   Antenna/baseline to include
C      LAN      I   test antenna/baseline
C   Output:
C      EDOANT   L   True if data included
C-----------------------------------------------------------------------
      INTEGER   ALL, IAN, LAN
C
      INTEGER   LA1, LA2
C-----------------------------------------------------------------------
C                                       all
      IF (ALL.EQ.0) THEN
         EDOANT = .TRUE.
C                                       one only
      ELSE IF (ALL.LT.0) THEN
         EDOANT = IAN.EQ.LAN
C                                       one antenna, >1 baseline
      ELSE
         CALL EDANTS (LAN, LA1, LA2)
         EDOANT = (ALL.EQ.LA1) .OR. (ALL.EQ.LA2)
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDPLOT (IERR)
C-----------------------------------------------------------------------
C   plots the data and optionally the full data at the top
C   This routine sets plot coordinates, channels, ... and calls EDPLT1
C   to plot each portion of the plot.
C   Outputs:
C      IERR     I       Error code: > 0 quit
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I, J, N, ITRIM, ITT(4), JERR, HCS, LCODE
      REAL      FRAC
      CHARACTER STRING*80, CCPREF*5
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
C                                       Recompute differences if needed
      IF (DDTYPE.EQ.'TY') THEN
         LCODE = 2
      ELSE IF (DDTYPE.EQ.'UV') THEN
         LCODE = 3
      ELSE
         LCODE = 99
         END IF
      IF (((LTYPE.EQ.LCODE) .OR. (DOCOMP .AND. (LTYPE2.EQ.LCODE))) .AND.
     *   (FLAGED)) THEN
         MSGTXT = 'Computing difference data, then plotting'
         CALL MSGWRT (2)
         IF (DDTYPE.EQ.'TY') CALL EDIFTY
         IF (DDTYPE.EQ.'UV') CALL EDIFUV
      ELSE IF ((FLAGED) .AND. (DDTYPE.EQ.'SY') .AND. (((LTYPE.GE.5)
     *   .AND. (LTYPE.LE.8)) .OR. ((DOCOMP) .AND. (LTYPE2.GE.5) .AND.
     *   (LTYPE2.LE.8)))) THEN
         MSGTXT = 'Computing difference data, then plotting'
         CALL MSGWRT (2)
         CALL EDIFSY
      ELSE
         MSGTXT = 'Plotting:'
         CALL MSGWRT (2)
         END IF
      HCS = (CSIZE(2) + 1) / 2
      FRAC = 2.0 - 1.0 / NUMPLT
      IF (DOCOMP) THEN
         IF (NUMPLT.GT.1) THEN
            N = (TOPLOT - LTVWND(2) - 4*CSIZE(2) - 7*HCS - LEDG) /
     *         (NUMPLT+FRAC)
         ELSE
            N = (TOPLOT - LTVWND(2) - 3*CSIZE(2) - 5*HCS - LEDG) /
     *         (NUMPLT+FRAC)
            END IF
      ELSE
         N = (TOPLOT - LTVWND(2) - 2*CSIZE(2) - 3*HCS - LEDG) /
     *      (NUMPLT+FRAC-1.0)
         END IF
      J = LEDG + 2 * CSIZE(2) + 3 * HCS + LTVWND(2)
      XYPLOT(1,1) = LTVWND(1) + (17+CEDG) * CSIZE(1) + 6
      XYPLOT(3,1) = LTVWND(3) - 18 * CSIZE(1) - 1 - (CSIZE(1)+1)/2
      XYPLOT(2,1) = J
      XYPLOT(4,1) = J + FRAC*N - 1
      J = J + N*FRAC
      IF (DOCOMP) THEN
         J = J + CSIZE(2) + 2 * HCS
         EXPLOT(2) = J
         EXPLOT(4) = EXPLOT(2) + N - 1
         EXPLOT(1) = XYPLOT(1,1)
         EXPLOT(3) = XYPLOT(3,1)
         J = J + N + CSIZE(2) + 2 * HCS
         END IF
      DO 10 I = 2,NUMPLT
         XYPLOT(1,I) = XYPLOT(1,1)
         XYPLOT(3,I) = XYPLOT(3,1)
         XYPLOT(2,I) = J
         J = J + N
         XYPLOT(4,I) = J - 1
 10      CONTINUE
C                                       set sub window
      IF (TIME2.LT.TIME1) THEN
         TIME1 = 1
         TIME2 = TIMEM-1
         END IF
C                                       Do plots: top
C                                       Edit data: set flags
      CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
C                                       comparison plot then main
      IF (DOCOMP) THEN
         CALL RCOPY (2, DPIXR(1,LTYPE2), APIXR2)
         CALL EDPLT1 (PLTAN(1), 2, EXPLOT, GRSEL(4), GRSEL(5),
     *      GRSEL(7), TIME1, TIME2, 2, APIXR2, IERR)
         IF (IERR.GT.0) GO TO 980
         CALL RCOPY (2, DPIXR(1,LTYPE), APIXR(1,1))
         CALL EDPLT1 (PLTAN(1), 1, XYPLOT(1,1), -GRSEL(4), -GRSEL(5),
     *      -GRSEL(7), TIME1, TIME2, 2, APIXR(1,1), IERR)
C                                       main plot
      ELSE
         CALL RCOPY (2, DPIXR(1,LTYPE), APIXR(1,1))
         CALL EDPLT1 (PLTAN(1), 1, XYPLOT(1,1), GRSEL(4), GRSEL(5),
     *      GRSEL(7), TIME1, TIME2, 2, APIXR(1,1), IERR)
         END IF
      IF (IERR.GT.0) GO TO 980
      CCPREF = CPREF(2,1)
      CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       extra plots: erase needed anyway
      CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
      CALL TVDOPR (TVNAME, 'GRCL', GRSEL(6), IERR)
      IF (IERR.NE.0) GO TO 980
      IF (NUMPLT.GT.1) THEN
         DO 20 I = 2,NUMPLT
            CALL RCOPY (2, DPIXR(1,LTYPE), APIXR(1,I))
            CALL EDPLT1 (PLTAN(I), 1, XYPLOT(1,I), -GRSEL(6),
     *         -GRSEL(5), -GRSEL(7), TIME1, TIME2, 3, APIXR(1,I), IERR)
            IF (IERR.NE.0) GO TO 980
 20         CONTINUE
         END IF
C                                       Main label
      J = XYPLOT(4,NUMPLT) + HCS + 1
      IF ((NUMPLT.EQ.1) .AND. (DOCOMP)) J = EXPLOT(4) + HCS + 1
      I = (XYPLOT(1,1) + XYPLOT(3,1) - CSIZE(1)*DDSLEN) / 2
      CALL TVDCHR (TVNAME, I, J, 0, 0, GRSEL(4), DDSTR(:DDSLEN), IERR)
      IF (IERR.NE.0) GO TO 980
C                                       bottom label
      I = ITRIM (DTYPE(LTYPE))
      STRING = DTYPE(LTYPE)(:I) // ' in ' // CCPREF
      I = ITRIM (STRING)
      STRING(I+2:) = DUNITS(LTYPE)
      I = ITRIM (STRING)
      I = I + 4
      WRITE (STRING(I:),1000) IFNOW, POLNOW
      N = ITRIM (STRING)
      I = (XYPLOT(1,1) + XYPLOT(3,1) - CSIZE(1)*N) / 2
      J = LTVWND(2) + LEDG + HCS
      CALL TVDCHR (TVNAME, I, J, 0, 0, GRSEL(4), STRING(:N), IERR)
      IF (IERR.NE.0) GO TO 980
      IF (DOCOMP) THEN
         IF (NUMPLT.GT.1) THEN
            J = EXPLOT(4) + HCS + 1
            CALL TVDCHR (TVNAME, I, J, 0, 0, GRSEL(4), STRING(:N), IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         I = ITRIM (DTYPE(LTYPE2))
         STRING = DTYPE(LTYPE2)(:I) // ' in ' // CPREF(2,2)
         I = ITRIM (STRING)
         STRING(I+2:) = DUNITS(LTYPE2)
         I = ITRIM (STRING)
         I = I + 4
         WRITE (STRING(I:),1000) IFNOW, POLNOW
         N = ITRIM (STRING)
         I = (XYPLOT(1,1) + XYPLOT(3,1) - CSIZE(1)*N) / 2
         J = XYPLOT(4,1) + HCS + 1
         CALL TVDCHR (TVNAME, I, J, 0, 0, GRSEL(4), STRING(:N), IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       time range
      J = LTVWND(2) + LEDG + 2 * HCS + CSIZE(2)
      CALL TODHMS (TIMES(EDIPTR+TIME1), ITT)
      WRITE (STRING,1020) ITT
      CALL CHTRIM (STRING, 14, STRING, N)
      I = XYPLOT(1,1) + LEDG - (CSIZE(1) * N) / 2
      CALL TVDCHR (TVNAME, I, J, 0, 0, GRSEL(4), STRING(:N), IERR)
      IF (IERR.NE.0) GO TO 980
      CALL TODHMS (TIMES(EDIPTR+TIME2), ITT)
      WRITE (STRING,1020) ITT
      CALL CHTRIM (STRING, 14, STRING, N)
      I = XYPLOT(3,1) - LEDG - (CSIZE(1) * N + 1) / 2
      CALL TVDCHR (TVNAME, I, J, 0, 0, GRSEL(4), STRING(:N), IERR)
      IF (IERR.NE.0) GO TO 980
C                                       status strings
      PNDING = .FALSE.
      CALL EDPLST (GRSEL(6), IERR)
      IF (IERR.NE.0) GO TO 980
      CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
      GO TO 999
C
 980  MSGTXT = 'EDPLOT: ERROR PLOTTING THE TV SCREEN'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IF',I3,4X,'POLARIZATION',I2)
 1020 FORMAT (I5,'/',2(I2.2,':'),I2.2)
      END
      SUBROUTINE EDPLSS (GR, IERR)
C-----------------------------------------------------------------------
C   plot status strings from lower left corner of visible area
C   For SN/CL editing only Now enhanced for TY editing
C   Inputs:
C      GR     I   Graphics plane to use
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   GR, IERR
C
      INTEGER   I, J, HCS
      CHARACTER STRING*28
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      HCS = (CSIZE(2) + 1) / 2
      I = LTVWND(1) + CSIZE(1)
      J = LTVWND(2) + LEDG + HCS
C                                       smooth type/time
      IF ((DDTYPE.EQ.'SN') .OR. (DDTYPE.EQ.'CL')) THEN
         IF ((SMTYPE.EQ.'GAUS') .OR. (SMTYPE.EQ.'LINE'))THEN
            WRITE (STRING,1000) SMTYPE, SMTIME(2), SMTIME(3)
         ELSE
            WRITE (STRING,1001) SMTYPE, SMTIME(2)
            END IF
         IF (SMTYPE.NE.' ') THEN
            CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING, IERR)
            IF (IERR.NE.0) GO TO 980
            J = J + CSIZE(2) + HCS
            END IF
      ELSE IF (((DDTYPE.EQ.'TY') .OR. (DDTYPE.EQ.'SY')) .AND.
     *   (.NOT.UVFLAG) .AND. (SMTYPE.NE.' ')) THEN
         IF ((SMTYPE.EQ.'GAUS') .OR. (SMTYPE.EQ.'LINE'))THEN
            WRITE (STRING,1003) SMTIME(1), SMTYPE, SMTIME(2), SMTIME(3)
         ELSE
            WRITE (STRING,1003) SMTIME(1), SMTYPE, SMTIME(2)
            END IF
         CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING, IERR)
         IF (IERR.NE.0) GO TO 980
         J = J + CSIZE(2) + HCS
C                                       uv scan average
      ELSE IF ((DDTYPE.EQ.'TY') .OR. (DDTYPE.EQ.'SY') .OR.
     *   (DDTYPE.EQ.'UV')) THEN
         WRITE (STRING,1005) SMTIME(1)
         CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING, IERR)
         IF (IERR.NE.0) GO TO 980
         J = J + CSIZE(2) + HCS
         END IF
C                                       flag all ant
      IF (ALLANT.EQ.0) THEN
         STRING = 'DO ALL ANTENNAS'
      ELSE IF (ALLANT.GT.0) THEN
         STRING = 'DO ONE ANTENNA '
      ELSE
         STRING = 'DO ONE BASELINE '
         END IF
      CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING(:15), IERR)
      IF (IERR.NE.0) GO TO 980
      J = J + CSIZE(2) + HCS
C                                       flag all pol
      IF (POLMAX.GT.1) THEN
         IF (ALLPOL) THEN
            STRING = 'DO ALL POLARIZ'
         ELSE
            STRING = 'DO ONE POLARIZ'
            END IF
         CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING(:14), IERR)
         IF (IERR.NE.0) GO TO 980
         J = J + CSIZE(2) + HCS
         END IF
C                                       flag all IFs
      IF (DIMIF.GT.1) THEN
         IF (ALLIF.EQ.1) THEN
            STRING = 'DO ALL IFS'
         ELSE IF (ALLIF.EQ.-1) THEN
            STRING = 'DO ONE IF '
         ELSE
            WRITE (STRING,1010) FGIFRS
            END IF
         CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING(:14), IERR)
         IF (IERR.NE.0) GO TO 980
         J = J + CSIZE(2) + HCS
         END IF
C                                       flag all times
      IF (ALLTIM) THEN
         STRING = 'DO ALL TIMES'
      ELSE
         STRING = 'DO ONE FRAME'
         END IF
      CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING(:14), IERR)
      IF (IERR.NE.0) GO TO 980
      J = J + CSIZE(2) + HCS
C                                       source flagging
      IF (ALLSOR) THEN
         STRING = 'DO ALL SOURCES'
      ELSE
         STRING = 'DO ONE SOURCE'
         END IF
      CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING(:14), IERR)
      IF (IERR.NE.0) GO TO 980
      J = J + CSIZE(2) + HCS
C                                       Pending load?
      IF (PNDING) THEN
         STRING = 'LOAD PENDING'
      ELSE
         STRING = ' '
         END IF
      CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING(:12), IERR)
      IF (IERR.NE.0) GO TO 980
      J = J + CSIZE(2) + HCS
      GO TO 999
C
 980  MSGTXT = 'EDPLSS: ERROR PLOTTING CL/SN STATUS STRINGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A4,2F6.1,' SEC')
 1001 FORMAT (A4,F8.1,' SECONDS')
 1003 FORMAT ('SCAN',F7.1,1X,A4,2F6.0)
 1005 FORMAT ('SCAN',F7.1,' SEC LONG')
 1010 FORMAT ('DO IFS',I3,' -',I3)
      END
      SUBROUTINE EDPLST (GR, IERR)
C-----------------------------------------------------------------------
C   plot status strings from lower left corner of visible area
C   Inputs:
C      GR     I   Graphics plane to use
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   GR, IERR
C
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
C                                       call by type
      IF ((DDTYPE.EQ.'SN') .OR. (DDTYPE.EQ.'CL')) THEN
         CALL EDPLSS (GR, IERR)
      ELSE IF (DDTYPE.EQ.'TY') THEN
         CALL EDPLSS (GR, IERR)
      ELSE IF (DDTYPE.EQ.'SY') THEN
         CALL EDPLSS (GR, IERR)
      ELSE IF (DDTYPE.EQ.'UV') THEN
         CALL EDPLSS (GR, IERR)
      ELSE
         IERR = 2
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDPLT1 (IAN, IPRM, CORN, GR1, GR2, GR3, IT1, IT2,
     *   DOTICS, LPIXR, IERR)
C-----------------------------------------------------------------------
C   Plots one antenna/baseline worth of data
C   Inputs:
C      IAN      I       Antenna number to plot
C      IPRM     I       First observable or second
C      CORN     I(4)    TV corners for this plot
C      GR1      I       Graphics channel (> 0 => zero it first, abs val)
C      GR2      I       Graphics channel for flagged data (0 none, > 0
C                       zero it first, uses abs value)
C      GR3      I       Graphics channel for 2nd uv used when DO2UV
C                       (> 0 => zero it first)
C      IT1      I       First time index into data
C      IT2      I       Last time index into data
C      DOTICS   I       Do tick marks (0,1), Y axis labels (1)
C   In/Out:
C      LPIXR    R(2)    Pixrange to use - set to used of input LPIXR(2)
C                          <= LPIXR(1)
C   Output:
C      IERR     I       error code: o okay
C-----------------------------------------------------------------------
      INTEGER   IAN, IPRM, CORN(4), GR1, GR2, GR3, IT1, IT2, DOTICS,
     *   IERR
      REAL      LPIXR(2)
C
      LONGINT   LPTR, JPTR
      INTEGER   IG1, IG2, IX(5), IY(5), I, ITRIM, J, IT, LT, IG3, LTY,
     *    LF, LF1, LF2, LP, LP1, LP2
      REAL      V, XS, YS, HOUR, PSCALE(2), WT, V1, V2, SIG, LLPIXR(2)
      LOGICAL   DOE, OK, DO3
      CHARACTER STRING*8
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
      LTY = LTYPE
      IF (IPRM.EQ.2) LTY = LTYPE2
      DOE = DOERRB
      IERR = 2
      IG1 = ABS (GR1)
      IG2 = ABS (GR2)
      IF (IG1.EQ.0) GO TO 999
      DO3 = (IG1.EQ.GRSEL(4))
      IG3 = 0
      IF (DO2ND) IG3 = ABS (GR3)
      IF (POLNOW.EQ.0) THEN
         LP1 = 1
         LP2 = 2
      ELSE
         LP1 = POLNOW
         LP2 = POLNOW
         END IF
      IF (IFNOW.EQ.0) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
C                                       zero the graphics memories
      IF (GR1.GT.0) THEN
         CALL TVDOPR (TVNAME, 'GRCL', GR1, IERR)
         IF (IERR.NE.0) GO TO 999
         IF ((DO3COL) .AND. (DO3)) THEN
            CALL TVDOPR (TVNAME, 'TVCL', CPLANE, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TVDOPR (TVNAME, 'TVCL', CPLANE+1, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TVDOPR (TVNAME, 'TVCL', CPLANE+2, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
      IF (GR2.GT.0) THEN
         CALL TVDOPR (TVNAME, 'GRCL', GR2, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IF ((DO2ND) .AND. (GR3.GT.0)) THEN
         CALL TVDOPR (TVNAME, 'GRCL', GR3, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       border line
      IX(1) = CORN(1)
      IX(2) = CORN(3)
      IX(3) = IX(2)
      IX(4) = IX(1)
      IX(5) = IX(1)
      IY(1) = CORN(2)
      IY(2) = IY(1)
      IY(3) = CORN(4)
      IY(4) = IY(3)
      IY(5) = IY(1)
      CALL TVDLIN (TVNAME, 1, IG1, 5, IX, IY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       antenna number
      IF (IAN.LE.255) THEN
         WRITE (STRING,1000) IAN
         I = 6
         IF (IAN.LT.100) I = 7
         IF (IAN.LT.10) I = 8
C                                       baseline
      ELSE
         I = IAN / 256
         J = IAN - I * 256
         WRITE (STRING,1001) I, J
         I = 4
         END IF
      IX(1) = CORN(3) - (10-I) * CSIZE(1)
      IY(1) = CORN(4) - 2 * CSIZE(2)
      CALL TVDCHR (TVNAME, IX, IY, 0, 0, IG1, STRING(I:), IERR)
      IF (IERR.NE.0) GO TO 999
C                                       find max min
      IF (LPIXR(2).LE.LPIXR(1)) THEN
         CALL EDMXMN (IAN, LTY, LLPIXR, IERR)
         IF (IERR.NE.0) THEN
            I = ITRIM (DTYPE(LTY))
            IF (IAN.LE.255) THEN
               WRITE (MSGTXT,1010) DTYPE(LTY)(:I), IAN
            ELSE
               WRITE (MSGTXT,1011) DTYPE(LTY)(:I), STRING(4:)
               END IF
            CALL MSGWRT (6)
            LPIXR(1) = 0.0
            LPIXR(2) = 1.0
            IERR = 0
         ELSE
            IF ((LPIXR(1).EQ.0.0) .AND. (LPIXR(2).LT.0.0) .AND.
     *         (LTY.EQ.1)) THEN
               LPIXR(2) = LLPIXR(2)
               LPIXR(1) = MIN (0.0, LLPIXR(1))
            ELSE
               LPIXR(1) = LLPIXR(1)
               LPIXR(2) = LLPIXR(2)
               END IF
            END IF
      ELSE IF (LTY.EQ.2) THEN
         LLPIXR(1) = LPIXR(1)
         LLPIXR(2) = LPIXR(2)
         CALL EDMXMT (IAN, LTY, LLPIXR, IERR)
         IERR = 0
         END IF
C                                       loop through data to plot
      LPTR = DPTR
      J = LTY * DIMIF
      XS = (CORN(3) - CORN(1) - 2.*LEDG) / REAL (IT2 - IT1)
      YS = (CORN(4) - CORN(2) - 2.*LEDG) / (LPIXR(2) - LPIXR(1))
      DO 100 I = 1,MAXREC
         IF ((IAN.EQ.EDCORI(LPTR)) .AND. (IT1.LE.EDCORI(LPTR+1)) .AND.
     *      (IT2.GE.EDCORI(LPTR+1))) THEN
            DO 90 LF = LF1,LF2
               DO 80 LP = LP1,LP2
                  CCOLOR = (LF-1) * POLMAX + LP
                  JPTR = DIMPRM + (LP-1) * DIMIF * DIMDAT
                  IF (DIMIF.GT.1) JPTR = JPTR + LF - BIF
                  IF (EDCORE(LPTR+JPTR).NE.0.0) THEN
                     V = EDCORE(LPTR+JPTR+J)
                     IF ((V.NE.FBLANK) .AND. (V.GE.LPIXR(1)) .AND.
     *                  (V.LE.LPIXR(2))) THEN
                        OK = .FALSE.
                        IF (DOE) THEN
                           WT = EDCORE(LPTR+JPTR)
                           IF ((WT.NE.FBLANK) .AND. (ABS(WT).GT.1.E-6))
     *                        THEN
                              CALL EDGSIG (LTY, WT,
     *                           EDCORE(LPTR+JPTR+DIMIF), SIG)
                              V2 = MAX (LPIXR(1), V - SIG)
                              V1 = MIN (LPIXR(2), V + SIG)
                              IY(1) = (V1 - LPIXR(1)) * YS + CORN(2) +
     *                           0.5 + LEDG
                              IY(2) = (V2 - LPIXR(1)) * YS + CORN(2) +
     *                           0.5 + LEDG
                              OK = (IY(1)-IY(2)).GE.2
                              END IF
                           END IF
                        IF (.NOT.OK) THEN
                           IY(1) = (V - LPIXR(1)) * YS + CORN(2) + 1.5 +
     *                        LEDG
                           IY(2) = IY(1) - 2
                           END IF
                        IX(1) = (EDCORI(LPTR+1)-IT1) * XS + CORN(1) +
     *                     0.5 + LEDG
                        IX(2) = IX(1)
                        IF (EDCORE(LPTR+JPTR).GT.0.0) THEN
                           IF (DO3) THEN
                              CALL ED3LIN (1, IG1, 2, IX, IY, IERR)
                           ELSE
                              CALL TVDLIN (TVNAME, 1, IG1, 2, IX, IY,
     *                           IERR)
                              END IF
                        ELSE IF (IG2.GT.0) THEN
                           CALL TVDLIN (TVNAME, 1, IG2, 2, IX, IY, IERR)
                           END IF
                        IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 999
                        END IF
                     END IF
C                                       weights for 2nd not available
                  IF ((IG3.GT.0) .AND. (EDCORE(LPTR+JPTR).GT.0.0)) THEN
                     V = EDCORE(LPTR+JPTR+J+4*DIMIF)
                     IF ((V.NE.FBLANK) .AND. (V.GE.LPIXR(1)) .AND.
     *                  (V.LE.LPIXR(2))) THEN
                        IY(1) = (V - LPIXR(1)) * YS + CORN(2) + 1.5 +
     *                     LEDG
                        IY(2) = IY(1) - 2
                        IX(1) = (EDCORI(LPTR+1)-IT1) * XS + CORN(1) +
     *                     0.5 + LEDG
                        IX(2) = IX(1)
                        CALL TVDLIN (TVNAME, 1, IG3, 2, IX, IY, IERR)
                        IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 999
                        END IF
                     END IF
 80               CONTINUE
 90            CONTINUE
            END IF
         LPTR = LPTR + DIMREC
 100     CONTINUE
      IERR = 0
C                                       Time ticks
      IF (DOTICS.GE.0) THEN
         HOUR = 1.0 / 24.0
         LT = TIMES(EDIPTR+IT1) / HOUR
         IY(1) = CORN(2)
         IY(2) = IY(1) + LEDG
         IY(3) = CORN(4)
         IY(4) = IY(3) - LEDG
         DO 120 I = IT1,IT2
 110        IT = TIMES(EDIPTR+I) / HOUR
            IF (IT.GT.LT) THEN
               LT = LT + 1
               V = (LT * HOUR - TIMES(EDIPTR+I-1)) / MAX (1.E-8,
     *            TIMES(EDIPTR+I) - TIMES(EDIPTR+I-1)) + I - 1
               IX(1) = (V - IT1) * XS + CORN(1) + 0.5 + LEDG
               IX(2) = IX(1)
               CALL TVDLIN (TVNAME, 1, IG1, 2, IX, IY, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL TVDLIN (TVNAME, 1, IG1, 2, IX, IY(3), IERR)
               IF (IERR.NE.0) GO TO 999
               GO TO 110
               END IF
 120        CONTINUE
         END IF
C                                       Y axis labeling
      IF (DOTICS.GT.0) THEN
         IF (DPLSCL(LTY).LE.0.0) DPLSCL(LTY) = 1.0
         PSCALE(1) = LPIXR(1) / DPLSCL(LTY)
         PSCALE(2) = LPIXR(2) / DPLSCL(LTY)
         CALL EDTICS (DOTICS, IPRM, CORN, IG1, PSCALE, IERR)
         IERR = MAX (IERR, 0)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I8)
 1001 FORMAT (3X,I2.2,'-',I2.2)
 1010 FORMAT ('NO VALID DATA OF TYPE ',A,' FOR ANTENNA',I6)
 1011 FORMAT ('NO VALID DATA OF TYPE ',A,' FOR BASELINE ',A)
      END
      SUBROUTINE EDPLTP (CORN, GR, IT, IT1, IT2, V, SIG, LPIXR, IERR)
C-----------------------------------------------------------------------
C   Plots one point
C   Inputs:
C      CORN     I(4)    TV corners for this plot
C      GR       I       Graphics channel (< 0 => erase the point)
C      IT       I       time index into data for the 1 point
C      IT1      I       first time plotted
C      IT2      I       last time plotted
C      V        R       value of point
C      SIG       R       1 sigma on V
C      LPIXR    R(2)    Pixrange used in plot
C   Output:
C      IERR     I       error code: 0 okay, -1 off plot
C-----------------------------------------------------------------------
      INTEGER   CORN(4), GR, IT, IT1, IT2, IERR
      REAL      V, SIG, LPIXR(2)
C
      INTEGER   IG1, IX(2), IY(2)
      REAL      XS, YS, V1, V2
      LOGICAL   OK, DO3
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IG1 = ABS (GR)
      IF (IG1.EQ.0) GO TO 999
      DO3 = (IG1.EQ.GRSEL(4))
      IERR = -1
C                                       loop through data to plot
      XS = (CORN(3) - CORN(1) - 2.*LEDG) / REAL (IT2 - IT1)
      YS = (CORN(4) - CORN(2) - 2.*LEDG) / (LPIXR(2) - LPIXR(1))
      IF ((V.GE.LPIXR(1)) .AND. (V.LE.LPIXR(2)) .AND. (IT1.LE.IT) .AND.
     *   (IT2.GE.IT) .AND. (V.NE.FBLANK)) THEN
         OK = .FALSE.
         IF ((SIG.NE.FBLANK) .AND. (SIG.NE.0.0)) THEN
            V1 = MIN (LPIXR(2), V+SIG)
            V2 = MAX (LPIXR(1), V-SIG)
            IY(1) = (V1 - LPIXR(1)) * YS + CORN(2) + 0.5 + LEDG
            IY(2) = (V2 - LPIXR(1)) * YS + CORN(2) + 0.5 + LEDG
            OK = IY(1)-IY(2).GE.2
            END IF
         IF (.NOT.OK) THEN
            IY(1) = (V - LPIXR(1)) * YS + CORN(2) + 1.5 + LEDG
            IY(2) = IY(1) - 2
            END IF
         IX(1) = (IT - IT1) * XS + CORN(1) + 0.5 + LEDG
         IX(2) = IX(1)
         IF (DO3) THEN
            IF (GR.GT.0) THEN
               CALL ED3LIN (1, IG1, 2, IX, IY, IERR)
            ELSE
               CALL ED3LIN (3, IG1, 2, IX, IY, IERR)
               END IF
         ELSE
            IF (GR.GT.0) THEN
               CALL TVDLIN (TVNAME, 1, IG1, 2, IX, IY, IERR)
            ELSE
               CALL TVDLIN (TVNAME, 3, IG1, 2, IX, IY, IERR)
               END IF
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDRRTR (IERR)
C-----------------------------------------------------------------------
C   interactive re-read of a time range
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), FLPF(2,2)
      REAL      TVXY(2), FLFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         MSGTXT = 'Hit button A to set the other side of the time range'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to re-read a time range and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to re-read a time range and return ' //
     *      'to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more reading'
         CALL MSGWRT (1)
         END IF
C                                       read cursor until button
      CALL EDFLAI (1, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       reread something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         CALL EDG1DA (FLTIMS, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      FIRST = -1
      CALL EDFLAI (0, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDRRTR: ERROR DOING TIME-RANGE RE-READING'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE EDRSAR (IERR)
C-----------------------------------------------------------------------
C   interactive restoring of an area: time range and flux range
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), NFL, POLOLD,
     *   IFOLD, IP1, IP2, IF1, IF2, FLPF(2,2), LLTIMS(2), FOP, IFL,
     *   PRTIMS(2)
      REAL      TVXY(2),  FLFLUX(2), PRFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      POLOLD = POLNOW
      IFOLD = IFNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (IFOLD.EQ.0) THEN
         IF1 = BIF
         IF2 = EIF
      ELSE
         IF1 = IFNOW
         IF2 = IFNOW
         END IF
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         MSGTXT = 'Hit button A to set other corner of the area'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to restore area and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to restore area and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more restores'
         CALL MSGWRT (1)
         END IF
C                                       read cursor until button
      POLNOW = POLOLD
      IFNOW = IFOLD
      CALL EDFLAI (4, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       restore  something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         FOP = 5
         IFL = 0
         PRTIMS(1) = FLTIMS(1)
         PRTIMS(2) = FLTIMS(2)
         PLTIF = IFOLD
         PLTPOL = POLOLD
C                                       Loop for each point
C                                       individually
         DO 40 IFNOW = IF1,IF2
            DO 30 POLNOW = IP1,IP2
               FLTIMS(1) = PRTIMS(1)
 20            CALL EDGTAP (PLTAN(1), LTYPE, FLTIMS, FLFLUX, IERR)
               IF (IERR.GT.0) GO TO 999
               IF (IERR.EQ.0) THEN
                  LLTIMS(1) = FLTIMS(1)
                  LLTIMS(2) = FLTIMS(1)
                  CALL EDFLAG ('UNFL', FOP, PLTAN(1), LTYPE, LLTIMS,
     *               FLFLUX, NFL, IERR)
                  IF (IERR.GT.0) GO TO 980
                  IFL = IFL + NFL
                  IF (NFL.GT.0) FOP = -5
                  FLTIMS(1) = FLTIMS(1) + 1
                  IF (FLTIMS(1).LE.FLTIMS(2)) GO TO 20
                  END IF
 30            CONTINUE
 40         CONTINUE
         PRFLUX(1) = FLFLUX(1) / DPLSCL(LTYPE)
         PRFLUX(2) = FLFLUX(2) / DPLSCL(LTYPE)
         WRITE (MSGTXT,1100) 'Restored', IFL, PRTIMS, PRFLUX
         CALL MSGWRT (2)
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (4, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDRSAR: ERROR DOING AREA RESTORES'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' points in TxF',2I7,' x',2(1PE10.2))
      END
      SUBROUTINE EDRSTI (IERR)
C-----------------------------------------------------------------------
C   interactive restoring (un-flagging) of a single time
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), NFL, POLOLD,
     *   IFOLD, IP1, IP2, IF1, IF2, FLPF(2,2)
      REAL      TVXY(2), FLFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      POLOLD = POLNOW
      IFOLD = IFNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (IFOLD.EQ.0) THEN
         IF1 = BIF
         IF2 = EIF
      ELSE
         IF1 = IFNOW
         IF2 = IFNOW
         END IF
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         MSGTXT = 'Hit buttons A or B to restore time and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to restore time and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more restores'
         CALL MSGWRT (1)
         END IF
C                                       read cursor until button
      POLNOW = POLOLD
      IFNOW = IFOLD
      CALL EDFLAI (0, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       restore something
      FLFLUX(1) = -1.E20
      FLFLUX(2) = 1.E20
      PLTIF = IFOLD
      PLTPOL = POLOLD
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         DO 40 IFNOW = IF1,IF2
            DO 30 POLNOW = IP1,IP2
               CALL EDFLAG ('UNFL', 1, PLTAN(1), LTYPE, FLTIMS, FLFLUX,
     *            NFL, IERR)
               IF (IERR.GT.0) GO TO 980
 30            CONTINUE
 40         CONTINUE
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (0, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDRSTI: ERROR DOING SINGLE-TIME RESTORES'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE EDVENT (TTY, IERR)
C-----------------------------------------------------------------------
C   interactive replacing of points with user-entered values
C   for TY editing only
C   Inputs:
C      TTY    I(2)   Terminal LUN and IND
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   TTY(2), IERR
C
      LONGINT   LPTR, IPTR
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), POLOLD,
     *   IFOLD, FLPF(2,2), LR, LR1, LR2, LA, LT, LF, LF1, LF2, LF0,
     *     JJPT, JJPT2, LPP, LPP1, LPP2, ITP, NPT, NPTB
      REAL      TVXY(2), FLFLUX(2), V, WT, W1, W2, AMP
      DOUBLE PRECISION DVAL
      LOGICAL   DOE, DOE2
      CHARACTER MSGBUF*80
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
      IF (UVFLAG) THEN
         MSGTXT = 'ENTER VALUES WORKS FOR EDITING TABLES ONLY'
         CALL MSGWRT (7)
         GO TO 999
C                                       even SY too messy
      ELSE IF (DDTYPE.NE.'TY') THEN
         MSGTXT = 'ENTER VALUES WORKS ONLY FOR TY TABLES'
         CALL MSGWRT (7)
         GO TO 999
         END IF
      CALL COPY (4, XYPLOT(1,1), CORN)
      DOE = DOERRB
      DOE2 = DOERRB .AND. DOCOMP
      W1 = 0.0
      W2 = 0.0
      TVBUTT = 1
      FIRST = 1
      POLOLD = POLNOW
      IFOLD = IFNOW
      NPT = 0
      NPTB = 0
      IF (POLOLD.EQ.0) THEN
         LPP1 = 1
         LPP2 = POLMAX
      ELSE
         LPP1 = POLNOW
         LPP2 = POLNOW
         END IF
      IF (IFOLD.EQ.0) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         MSGTXT = 'Hit buttons A or B to select point and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to select point and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more changes'
         CALL MSGWRT (1)
         END IF
C                                       read cursor until button
      POLNOW = POLOLD
      IFNOW = IFOLD
      CALL EDFLAI (5, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       process a point
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7)) THEN
         LR1 = EDCORI(PPTR+FLTIMS(1)-1)
         LR2 = EDCORI(PPTR+FLTIMS(2)) - 1
         LR2 = MIN (LR2, MAXREC)
         IF (DIMIF.GT.1) THEN
            LF0 = BIF
         ELSE
            LF0 = MAX (BIF, IFNOW)
            END IF
         IF (LR2.GE.LR1) THEN
            DO 50 LF = LF1,LF2
            DO 50 LPP = LPP1,LPP2
               CCOLOR = (LF - 1) * POLMAX + LPP
               IPTR = DIMPRM + (LPP-1) * DIMIF * DIMDAT + LF - LF0
               JJPT = IPTR + LTYPE * DIMIF
               IF (DOCOMP) JJPT2 = IPTR + LTYPE2 * DIMIF
               DO 30 LR = LR1,LR2
                  LPTR = DPTR + (LR - 1) * DIMREC
                  LA = EDCORI(LPTR)
                  LT = EDCORI(LPTR+1)
                  IF ((LA.EQ.PLTAN(1)) .AND. (FLTIMS(1).LE.LT) .AND.
     *               (FLTIMS(2).GE.LT)) THEN
C                                       change display
                     IF ((PLTAN(1).EQ.LA) .AND. ((PLTIF.EQ.LF)
     *                  .OR. (PLTIF.EQ.0)) .AND. ((PLTPOL.EQ.LPP)
     *                  .OR. (PLTPOL.EQ.0))) THEN
C                                       main
                        V = EDCORE(LPTR+JJPT)
                        WT = EDCORE(LPTR+IPTR)
                        AMP = EDCORE(LPTR+IPTR+DIMIF)
                        IF (DOE) CALL EDGSIG (LTYPE, WT, AMP, W1)
                        IF (DOE2) CALL EDGSIG (LTYPE2, WT, AMP, W2)
                        IF (GRSEL(4).GT.0) THEN
                           ITP = -GRSEL(4)
                           CALL EDPLTP (XYPLOT, ITP, LT, TIME1,
     *                        TIME2, V, W1, APIXR, IERR)
                           IF (IERR.GT.0) GO TO 980
                           END IF
C                                       flagged
                        IF (GRSEL(5).GT.0) THEN
                           ITP = -GRSEL(5)
                           CALL EDPLTP (XYPLOT, ITP, LT, TIME1,
     *                        TIME2, V, W1, APIXR, IERR)
                           IF (IERR.GT.0) GO TO 980
                           END IF
C                                       Comparison
                        IF (DOCOMP) V = EDCORE(LPTR+JJPT2)
                        IF ((GRSEL(4).GT.0) .AND. (DOCOMP)) THEN
                           ITP = -GRSEL(4)
                           CALL EDPLTP (EXPLOT, ITP, LT, TIME1,
     *                        TIME2, V, W2, APIXR2, IERR)
                           IF (IERR.GT.0) GO TO 980
                           END IF
C                                       flagged comparison
                        IF ((GRSEL(5).GT.0) .AND. (DOCOMP)) THEN
                           ITP = -GRSEL(5)
                           CALL EDPLTP (EXPLOT, ITP, LT, TIME1,
     *                        TIME2, V, W2, APIXR2, IERR)
                           IF (IERR.GT.0) GO TO 980
                           END IF
                        END IF
C                                       ask for value
                     WRITE (MSGBUF,1000) LF, LPP, LA, LT
                     CALL INQFLT (TTY, MSGBUF, 1, DVAL, IERR)
                     IF (IERR.GT.0) GO TO 980
                     IF (IERR.EQ.0) THEN
                        EDCORE(LPTR+IPTR+DIMIF) = DVAL
                        EDCORE(LPTR+IPTR) = 1.0
                        EDCORE(LPTR+IPTR+2*DIMIF) = 0.0
                        NPT = NPT + 1
                     ELSE
                        IERR = 0
                        MSGTXT = 'ERROR: ANSWER IGNORED'
                        CALL MSGWRT (6)
                        END IF
C                                       change display
                     IF ((PLTAN(1).EQ.LA) .AND. ((PLTIF.EQ.LF)
     *                  .OR. (PLTIF.EQ.0)) .AND. ((PLTPOL.EQ.LPP)
     *                  .OR. (PLTPOL.EQ.0))) THEN
C                                       main
                        V = EDCORE(LPTR+JJPT)
                        WT = EDCORE(LPTR+IPTR)
                        AMP = EDCORE(LPTR+IPTR+DIMIF)
                        IF (DOE) CALL EDGSIG (LTYPE, WT, AMP, W1)
                        IF (DOE2) CALL EDGSIG (LTYPE2, WT, AMP, W2)
                        IF ((GRSEL(4).GT.0) .AND. (WT.GT.0.0)) THEN
                           ITP = GRSEL(4)
                           CALL EDPLTP (XYPLOT, ITP, LT, TIME1,
     *                        TIME2, V, W1, APIXR, IERR)
                           IF (IERR.GT.0) GO TO 980
                           IF (IERR.LT.0) NPTB = NPTB + 1
                           END IF
C                                       flagged
                        IF ((GRSEL(5).GT.0) .AND. (WT.LE.0.0)) THEN
                           ITP = GRSEL(5)
                           CALL EDPLTP (XYPLOT, ITP, LT, TIME1,
     *                        TIME2, V, W1, APIXR, IERR)
                           IF (IERR.GT.0) GO TO 980
                           IF (IERR.LT.0) NPTB = NPTB + 1
                           END IF
C                                       Comparison
                        IF (DOCOMP) THEN
                           V = EDCORE(LPTR+JJPT2)
                           IF ((GRSEL(4).GT.0) .AND. (WT.GT.0.0)) THEN
                              ITP = GRSEL(4)
                              CALL EDPLTP (EXPLOT, ITP, LT, TIME1,
     *                           TIME2, V, W2, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
C                                       flagged comparison
                           IF ((GRSEL(5).GT.0) .AND. (WT.LE.0.0)) THEN
                              ITP = GRSEL(5)
                              CALL EDPLTP (EXPLOT, ITP, LT, TIME1,
     *                           TIME2, V, W2, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              IF (IERR.LT.0) NPTB = NPTB + 1
                              END IF
                           END IF
                        END IF
                     END IF
 30               CONTINUE
 50            CONTINUE
            END IF
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL EDFLAI (5, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (NPT.GT.0) THEN
         FLAGED = .TRUE.
         WRITE (MSGTXT,1050) NPT
         CALL MSGWRT (3)
         END IF
      IF (NPTB.GT.0) THEN
         WRITE (MSGTXT,1051) NPTB
         CALL MSGWRT (6)
         END IF
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDVENT: ERROR DOING SINGLE-VALUE CHANGES'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Enter TSYS value for IF',I3,' POL',I2,' ANT',I3,' TIME',
     *   I5)
 1050 FORMAT ('Replaced',I6,' samples with user-supplied values')
 1051 FORMAT ('WARNING:',I6,' NEW POINTS FELL OFF PLOT - USE REPLOT')
      END
      SUBROUTINE EDSLST (IERR)
C-----------------------------------------------------------------------
C   fills in the SLIST common variable with a list of source names if
C   there is a source table attached to the object or makes a default
C   list.
C   Outputs:
C      IERR      I         Error code
C   Output in common:
C      SLIST     C(*)*16   Source names
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'INCS:PAOOF.INC'
      LOGICAL   EXIST
      CHARACTER SUFILE*32, INKT(4)*8, INEXT*2, CDUMMY*1, STATUS*4,
     *   SOLLAB(2)*8, SOURCE*8, SNS(30)*16, SN*16
      INTEGER   VERS, DIM(7), DUMMY, SUROW, IRET, I, NR, IDSOU,
     *   SOLNUM(2), TYPE, NCL, MSGSAV
      DATA INKT /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
      DATA SOLLAB /'ID. NO. ','SOURCE  '/
C-----------------------------------------------------------------------
C                                       dummy list - no error returned
      IERR = 0
      DO 10 I = 1,NSRC
         WRITE (SLIST(I),1000) I
 10      CONTINUE
      SUNUMB = 1
      SN = ' '
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OGET (UVMAST, 'OBJECT', TYPE, DIM, DUMMY, SOURCE, IRET)
      IF (IRET.EQ.0) SN = SOURCE
      IRET = 0
      CALL OGET (UVMAST, 'CALEDIT.SOURCS', TYPE, DIM, DUMMY, SNS, IRET)
      IF ((IRET.EQ.0) .AND. (SNS(1).NE.' ') .AND. ((SNS(2).EQ.' ') .OR.
     *   (SNS(2).EQ.SNS(1)))) SN = SNS(1)
      IRET = 0
      MSGSUP = MSGSAV
      IF (SN.NE.' ') SLIST(1) = SN
C                                       Create SUFILE table object
      SUFILE = 'Source table for editing'
      CALL CREATE (SUFILE, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 990
C                                       copy adverbs to SUFILE
      CALL IN2OBJ (UVMAST, 4, INKT, INKT, SUFILE, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Extension type TBLTYPE
      INEXT = 'SU'
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT (SUFILE, 'TBLTYPE', OOACAR, DIM, DUMMY, INEXT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       version always 1
      VERS = 1
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (SUFILE, 'VER', OOAINT, DIM, VERS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OBFEXS (SUFILE, EXIST, IERR)
      IERR = 0
C                                       open
      IF (EXIST) THEN
         STATUS = 'READ'
         CALL TABOPN (SUFILE, STATUS, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL OGET (SUFILE, 'NROW', TYPE, DIM, NR, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 980
         NCL = 2
         CALL TABCOL (SUFILE, NCL, SOLLAB, SOLNUM, IRET)
         IF (IRET.NE.0) GO TO 980
C                                       read
         DO 30 I = 1,NR
            SUROW = I
            CALL TABDGT (SUFILE, SUROW, SOLNUM(1), TYPE, DIM, IDSOU,
     *         CDUMMY, IRET)
            IF (IRET.GT.0) GO TO 980
            IF ((IDSOU.GT.0) .AND. (IDSOU.LE.NSRC) .AND. (IRET.EQ.0))
     *         THEN
               CALL TABDGT (SUFILE, SUROW, SOLNUM(2), TYPE, DIM, DUMMY,
     *            SLIST(IDSOU), IRET)
               IF (IRET.NE.0) GO TO 980
               IF (SLIST(IDSOU).EQ.SN) SUNUMB = IDSOU
               END IF
 30         CONTINUE
C                                       close
         CALL TABCLO (SUFILE, IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
      CALL TABDES (SUFILE, IRET)
      IF (IRET.NE.0) GO TO 990
      GO TO 999
C                                       close
 980  CALL TABCLO (SUFILE, IRET)
      CALL TABDES (SUFILE, IRET)
C
 990  MSGTXT = 'ERROR OCCURRED WHILE MAKING SOURCE LIST - IGNORED'
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOURCE',I4.2)
      END
      SUBROUTINE EDSMTH (IAN, TYPE, FLTIMS, IERR)
C-----------------------------------------------------------------------
C   Smooth samples in specified range, restore them to plot, remove
C   them on flagged plot (if any)
C   Inputs:
C      IGS      I(4)   Graphics plane of data, extra data, flagged, top
C      IAN      I      Antenna to flag (if not ALLANT), main antenna in
C                      plots in any case
C      TYPE     I      Data type to smooth
C      FLTIMS   I(2)   Time array indices of flag window
C   Output:
C      IERR     I      Error code: 0 -> okay
C-----------------------------------------------------------------------
      INTEGER   IAN, TYPE, FLTIMS(2), IERR
C
      LONGINT   LPTR, IPTR, JPTR, JJPTR
      INTEGER   LR, NFL, LA, LT, IP1, IP2, IA1, IA2, IP, IA, NPT, NPTM,
     *   IR, LF, LF1, LF2, LF0, JPTR2, I, J, SORC
      REAL      VO, VN, VA, TIMELO, TIMEHI, TWIDTH(2), FCUT
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      DATA SORC, FCUT /-1, 0.001/
C-----------------------------------------------------------------------
C                                       pointers
      NFL = 0
      NPTM = SCMAXS / 4
      TWIDTH(1) = SMTIME(2) / (3600. * 24.)
      TWIDTH(2) = SMTIME(3) / (3600. * 24.)
      IF ((ALLPOL) .OR. (POLNOW.EQ.0)) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (ALLANT.EQ.0) THEN
         IA1 = 1
         IA2 = ANTMAX
      ELSE
         IA1 = IAN
         IA2 = IAN
         END IF
      IF ((ALLIF.EQ.1) .OR. (IFNOW.EQ.0)) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE IF (ALLIF.EQ.0) THEN
         LF1 = FGIFRS(1)
         LF2 = FGIFRS(2)
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
      IF (DIMIF.GT.1) THEN
         LF0 = BIF
      ELSE
         LF0 = MAX (IFNOW, BIF)
         END IF
      TIMELO = TIMES(EDIPTR+FLTIMS(1)) - TWIDTH(1) * 1.02
      TIMEHI = TIMES(EDIPTR+FLTIMS(2)) + TWIDTH(1) * 1.02
      DO 100 LF = LF1,LF2
      DO 100 IP = IP1,IP2
         CCOLOR = (LF - 1) * POLMAX + IP
         DO 95 IA = IA1,IA2
            NPT = 0
            IPTR = DIMPRM + (IP-1) * DIMIF * DIMDAT + LF - LF0
            JPTR = IPTR + TYPE * DIMIF
            JJPTR = IPTR + LTYPE * DIMIF
            IF (DOCOMP) JPTR2 = IPTR + LTYPE2 * DIMIF
            J = 0
            DO 10 I = 1,NUMPLT
               IF (IA.EQ.PLTAN(I)) J = I
 10            CONTINUE
            IF ((LF.NE.IFNOW) .AND. (IFNOW.GT.0)) J = 0
            IF ((IP.NE.POLNOW) .AND. (POLNOW.GT.0)) J = 0
            DO 20 LR = 1,MAXREC
               LPTR = DPTR + (LR - 1) * DIMREC
               LA = EDCORI(LPTR)
               LT = EDCORI(LPTR+1)
               IF ((IA.EQ.LA) .AND. (TIMELO.LE.TIMES(EDIPTR+LT)) .AND.
     *            (TIMEHI.GE.TIMES(EDIPTR+LT))) THEN
                  IF (NPT.GE.NPTM) THEN
                     MSGTXT = 'TIME RANGE TOO BROAD FOR MEMORY'
                     CALL MSGWRT (6)
                     IERR = 0
                     GO TO 999
                     END IF
                  EDCORI(SPTR+NPT) = LR
                  EDCORE(SPTR+NPT+NPTM) = TIMES(EDIPTR+LT)
                  IF (EDCORE(LPTR+IPTR).LE.0.0) THEN
                     EDCORE(SPTR+NPT+2*NPTM) = FBLANK
                  ELSE
                     EDCORE(SPTR+NPT+2*NPTM) = EDCORE(LPTR+JPTR)
                     END IF
                  NPT = NPT + 1
                  END IF
 20            CONTINUE
C                                       smooth it
            IF (NPT.GT.0) THEN
               IF (SMTYPE.EQ.'BOX') THEN
                  CALL BOXBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+3*NPTM))
               ELSE IF (SMTYPE.EQ.'MWF') THEN
                  CALL MWFBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+3*NPTM))
               ELSE IF ((SMTYPE.EQ.'GAUS') .OR. (SMTYPE.EQ.'LINE') .OR.
     *            (SMTYPE.EQ.'EXP ')) THEN
                  CALL FUNBSM (SMTYPE, FCUT, TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+3*NPTM))
               ELSE IF (SMTYPE.EQ.'2PTH') THEN
                  CALL TPTBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT, .TRUE.,
     *               EDCORE(SPTR+3*NPTM))
               ELSE
                  CALL TPTBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT, .FALSE.,
     *               EDCORE(SPTR+3*NPTM))
                  END IF
C                                       put it back
               DO 30 IR = 1,NPT
                  LR = EDCORI(SPTR+IR-1)
                  LPTR = DPTR + (LR - 1) * DIMREC
                  LT = EDCORI(LPTR+1)
C                                       adjust display
                  VN = EDCORE(SPTR+3*NPTM+IR-1)
                  VO = EDCORE(SPTR+2*NPTM+IR-1)
                  IF ((LT.GE.FLTIMS(1)) .AND. (LT.LE.FLTIMS(2)) .AND.
     *               (VO.NE.VN)) THEN
                     IF (VO.NE.FBLANK) WASFLG = MAX (0, WASFLG) + 1
                     VO = EDCORE(LPTR+JJPTR)
                     IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
                     NFL = NFL + 1
                     IF ((J.GT.0) .AND.
     *                  (EDCORE(LPTR+IPTR).NE.0.0)) THEN
C                                       erase from flagged
                        IF (EDCORE(LPTR+IPTR).LT.0.0) THEN
                           IF ((GRSEL(5).GT.0) .AND. (VO.NE.FBLANK))
     *                        THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(5), LT,
     *                           TIME1, TIME2, VO, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           IF ((GRSEL(5).GT.0) .AND. (DOCOMP) .AND.
     *                        (J.EQ.1) .AND. (VA.NE.FBLANK)) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
C                                       erase from not flagged
                        ELSE IF (J.EQ.1) THEN
                           IF ((GRSEL(4).GT.0) .AND. (VO.NE.FBLANK))
     *                        THEN
                              CALL EDPLTP (XYPLOT, -GRSEL(4), LT, TIME1,
     *                           TIME2, VO, 0.0, APIXR, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           IF ((GRSEL(4).GT.0) .AND. (DOCOMP) .AND.
     *                        (VA.NE.FBLANK)) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF ((GRSEL(6).GT.0) .AND. (VO.NE.FBLANK))
     *                        THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(6), LT,
     *                           TIME1, TIME2, VO, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
C                                       blanked
                     IF (VN.EQ.FBLANK) THEN
                        EDCORE(LPTR+IPTR) = - ABS (EDCORE(LPTR+IPTR))
                        IF (J.GT.0) THEN
                           IF ((GRSEL(5).GT.0) .AND. (VO.NE.FBLANK))
     *                        THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(5), LT,
     *                           TIME1, TIME2, VO, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           IF ((GRSEL(5).GT.0) .AND. (DOCOMP) .AND.
     *                        (J.EQ.1) .AND. (VA.NE.FBLANK)) THEN
                              CALL EDPLTP (EXPLOT, GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
C                                       good now
                     ELSE
                        EDCORE(LPTR+IPTR) =  MAX (0.0001,
     *                     ABS (EDCORE(LPTR+IPTR)))
                        EDCORE(LPTR+JPTR) = VN
                        VO = EDCORE(LPTR+JJPTR)
                        IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
C                                       put on TV
                        IF ((J.EQ.1) .AND. (GRSEL(4).GT.0)) THEN
                           CALL EDPLTP (XYPLOT, GRSEL(4), LT, TIME1,
     *                        TIME2, VO, 0.0, APIXR, IERR)
                           IF (IERR.GT.0) GO TO 980
                           IF ((DOCOMP) .AND. (VA.NE.FBLANK)) THEN
                              CALL EDPLTP (EXPLOT, GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        IF ((J.GT.1) .AND. (GRSEL(6).GT.0)) THEN
                           CALL EDPLTP (XYPLOT(1,J), GRSEL(6), LT,
     *                        TIME1, TIME2, VO, 0.0, APIXR(1,J), IERR)
                           IF (IERR.GT.0) GO TO 980
                           END IF
                        END IF
                     END IF
 30               CONTINUE
               END IF
 95         CONTINUE
 100     CONTINUE
C                                       debug message
      WRITE (MSGTXT,1100) NFL, FLTIMS
      CALL MSGWRT (2)
      IF (NFL.GT.0) FLAGED = .TRUE.
      IERR = 0
      GO TO 999
C
 980  MSGTXT = 'EDSMTH: ERROR UPDATING THE TV DISPLAY FOR DATA ' //
     *   'SMOOTHING'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Altered',I6,' points in T index range',2I7)
      END
      SUBROUTINE EDSMSY (IAN, FLTIMS, IERR)
C-----------------------------------------------------------------------
C   Smooth samples in specified range, restore them to plot, remove
C   them on flagged plot (if any) - does 3 types data for SY
C   Inputs:
C      IAN      I      Antenna to flag (if not ALLANT), main antenna in
C                      plots in any case
C      FLTIMS   I(2)   Time array indices of flag window
C   Output:
C      IERR     I      Error code: 0 -> okay
C-----------------------------------------------------------------------
      INTEGER   IAN, FLTIMS(2), IERR
C
      LONGINT   LPTR, IPTR, JPTR, JJPTR
      INTEGER   LR, NFL, LA, LT, IP1, IP2, IA1, IA2, IP, IA, NPT, NPTM,
     *   IR, LF, LF1, LF2, LF0, JPTR2, I, J, SORC, TYPE, CALTYP
      REAL      VO1, VO2, VN1, VN2, VA, TIMELO, TIMEHI, TWIDTH(2), FCUT,
     *   VO, VN3, VO3, TC
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      DATA SORC, FCUT /-1, 0.001/
C-----------------------------------------------------------------------
C                                       pointers
      TYPE = 1
      NFL = 0
      NPTM = SCMAXS / 8
      TWIDTH(1) = SMTIME(2) / (3600. * 24.)
      TWIDTH(2) = SMTIME(3) / (3600. * 24.)
      IF ((ALLPOL) .OR. (POLNOW.EQ.0)) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      IF (ALLANT.EQ.0) THEN
         IA1 = 1
         IA2 = ANTMAX
      ELSE
         IA1 = IAN
         IA2 = IAN
         END IF
      IF ((ALLIF.EQ.1) .OR. (IFNOW.EQ.0)) THEN
         LF1 = BIF
         LF2 = EIF
      ELSE IF (ALLIF.EQ.0) THEN
         LF1 = FGIFRS(1)
         LF2 = FGIFRS(2)
      ELSE
         LF1 = IFNOW
         LF2 = IFNOW
         END IF
      IF (DIMIF.GT.1) THEN
         LF0 = BIF
      ELSE
         LF0 = MAX (IFNOW, BIF)
         END IF
      TIMELO = TIMES(EDIPTR+FLTIMS(1)) - TWIDTH(1) * 1.02
      TIMEHI = TIMES(EDIPTR+FLTIMS(2)) + TWIDTH(1) * 1.02
      DO 100 LF = LF1,LF2
      DO 100 IP = IP1,IP2
         CCOLOR = (LF - 1) * POLMAX + IP
         DO 95 IA = IA1,IA2
            NPT = 0
            IPTR = DIMPRM + (IP-1) * DIMIF * DIMDAT + LF - LF0
            JPTR = IPTR + TYPE * DIMIF
            JJPTR = IPTR + LTYPE * DIMIF
            IF (DOCOMP) JPTR2 = IPTR + LTYPE2 * DIMIF
            J = 0
            DO 10 I = 1,NUMPLT
               IF (IA.EQ.PLTAN(I)) J = I
 10            CONTINUE
            IF ((LF.NE.IFNOW) .AND. (IFNOW.GT.0)) J = 0
            IF ((IP.NE.POLNOW) .AND. (POLNOW.GT.0)) J = 0
            DO 20 LR = 1,MAXREC
               LPTR = DPTR + (LR - 1) * DIMREC
               LA = EDCORI(LPTR)
               LT = EDCORI(LPTR+1)
               IF ((IA.EQ.LA) .AND. (TIMELO.LE.TIMES(EDIPTR+LT)) .AND.
     *            (TIMEHI.GE.TIMES(EDIPTR+LT))) THEN
                  IF (NPT.GE.NPTM) THEN
                     MSGTXT = 'TIME RANGE TOO BROAD FOR MEMORY'
                     CALL MSGWRT (6)
                     IERR = 0
                     GO TO 999
                     END IF
                  EDCORI(SPTR+NPT) = LR
                  EDCORE(SPTR+NPT+NPTM) = TIMES(EDIPTR+LT)
                  IF (EDCORE(LPTR+IPTR).LE.0.0) THEN
                     EDCORE(SPTR+NPT+2*NPTM) = FBLANK
                     EDCORE(SPTR+NPT+3*NPTM) = FBLANK
                     EDCORE(SPTR+NPT+4*NPTM) = FBLANK
                  ELSE
                     EDCORE(SPTR+NPT+2*NPTM) = EDCORE(LPTR+JPTR)
                     EDCORE(SPTR+NPT+3*NPTM) = EDCORE(LPTR+JPTR+DIMIF)
                     EDCORE(SPTR+NPT+4*NPTM) = EDCORE(LPTR+JPTR+2*DIMIF)
                     END IF
                  NPT = NPT + 1
                  END IF
 20            CONTINUE
C                                       smooth it
            IF (NPT.GT.0) THEN
               IF (SMTYPE.EQ.'BOX') THEN
                  CALL BOXBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+5*NPTM))
                  CALL BOXBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+3*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+6*NPTM))
                  CALL BOXBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+4*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+7*NPTM))
               ELSE IF (SMTYPE.EQ.'MWF') THEN
                  CALL MWFBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+5*NPTM))
                  CALL MWFBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+3*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+6*NPTM))
                  CALL MWFBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+4*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+7*NPTM))
               ELSE IF ((SMTYPE.EQ.'GAUS') .OR. (SMTYPE.EQ.'LINE') .OR.
     *            (SMTYPE.EQ.'EXP ')) THEN
                  CALL FUNBSM (SMTYPE, FCUT, TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+5*NPTM))
                  CALL FUNBSM (SMTYPE, FCUT, TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+3*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+6*NPTM))
                  CALL FUNBSM (SMTYPE, FCUT, TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+4*NPTM), SORC, FBLANK, NPT,
     *               EDCORE(SPTR+7*NPTM))
               ELSE IF (SMTYPE.EQ.'2PTH') THEN
                  CALL TPTBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT, .TRUE.,
     *               EDCORE(SPTR+5*NPTM))
                  CALL TPTBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+3*NPTM), SORC, FBLANK, NPT, .TRUE.,
     *               EDCORE(SPTR+6*NPTM))
                  CALL TPTBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+4*NPTM), SORC, FBLANK, NPT, .TRUE.,
     *               EDCORE(SPTR+7*NPTM))
               ELSE
                  CALL TPTBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+2*NPTM), SORC, FBLANK, NPT, .FALSE.,
     *               EDCORE(SPTR+5*NPTM))
                  CALL TPTBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+3*NPTM), SORC, FBLANK, NPT, .FALSE.,
     *               EDCORE(SPTR+6*NPTM))
                  CALL TPTBSM (TWIDTH, EDCORE(SPTR+NPTM),
     *               EDCORE(SPTR+4*NPTM), SORC, FBLANK, NPT, .FALSE.,
     *               EDCORE(SPTR+7*NPTM))
                  END IF
C                                       put it back
               DO 30 IR = 1,NPT
                  LR = EDCORI(SPTR+IR-1)
                  LPTR = DPTR + (LR - 1) * DIMREC
                  LT = EDCORI(LPTR+1)
                  CALTYP = EDCORI(LPTR+3)
C                                       adjust display
                  VN1 = EDCORE(SPTR+5*NPTM+IR-1)
                  VN2 = EDCORE(SPTR+6*NPTM+IR-1)
                  VN3 = EDCORE(SPTR+7*NPTM+IR-1)
                  VO1 = EDCORE(SPTR+2*NPTM+IR-1)
                  VO2 = EDCORE(SPTR+3*NPTM+IR-1)
                  VO3 = EDCORE(SPTR+4*NPTM+IR-1)
                  IF ((LT.GE.FLTIMS(1)) .AND. (LT.LE.FLTIMS(2)) .AND.
     *               ((VO1.NE.VN1) .OR. (VO2.NE.VN2) .OR. (VO3.NE.VN3)))
     *               THEN
                     IF (VO1.NE.FBLANK) WASFLG = MAX (0, WASFLG) + 1
                     IF (VO2.NE.FBLANK) WASFLG = MAX (0, WASFLG) + 1
                     IF (VO3.NE.FBLANK) WASFLG = MAX (0, WASFLG) + 1
                     VO = EDCORE(LPTR+JJPTR)
                     IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
                     NFL = NFL + 1
                     IF ((J.GT.0) .AND.
     *                  (EDCORE(LPTR+IPTR).NE.0.0)) THEN
C                                       erase from flagged
                        IF (EDCORE(LPTR+IPTR).LT.0.0) THEN
                           IF ((GRSEL(5).GT.0) .AND. (VO.NE.FBLANK))
     *                        THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(5), LT,
     *                           TIME1, TIME2, VO, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           IF ((GRSEL(5).GT.0) .AND. (DOCOMP) .AND.
     *                        (J.EQ.1) .AND. (VA.NE.FBLANK)) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
C                                       erase from not flagged
                        ELSE IF (J.EQ.1) THEN
                           IF ((GRSEL(4).GT.0) .AND. (VO.NE.FBLANK))
     *                        THEN
                              CALL EDPLTP (XYPLOT, -GRSEL(4), LT, TIME1,
     *                           TIME2, VO, 0.0, APIXR, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           IF ((GRSEL(4).GT.0) .AND. (DOCOMP) .AND.
     *                        (VA.NE.FBLANK)) THEN
                              CALL EDPLTP (EXPLOT, -GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                        ELSE
                           IF ((GRSEL(6).GT.0) .AND. (VO.NE.FBLANK))
     *                        THEN
                              CALL EDPLTP (XYPLOT(1,J), -GRSEL(6), LT,
     *                           TIME1, TIME2, VO, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        END IF
C                                       blanked
                     IF (VN1.EQ.FBLANK) THEN
                        EDCORE(LPTR+IPTR) = - ABS (EDCORE(LPTR+IPTR))
                        IF (J.GT.0) THEN
                           IF ((GRSEL(5).GT.0) .AND. (VO.NE.FBLANK))
     *                        THEN
                              CALL EDPLTP (XYPLOT(1,J), GRSEL(5), LT,
     *                           TIME1, TIME2, VO, 0.0, APIXR(1,J),
     *                           IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           IF ((GRSEL(5).GT.0) .AND. (DOCOMP) .AND.
     *                        (J.EQ.1) .AND. (VA.NE.FBLANK)) THEN
                              CALL EDPLTP (EXPLOT, GRSEL(5), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
C                                       good now
                     ELSE
                        EDCORE(LPTR+IPTR) =  MAX (0.0001,
     *                     ABS (EDCORE(LPTR+IPTR)))
                        EDCORE(LPTR+JPTR) = VN1
                        EDCORE(LPTR+JPTR+DIMIF) = VN2
                        EDCORE(LPTR+JPTR+2*DIMIF) = VN3
                        IF (CALTYP.EQ.1) THEN
                           TC = TCAL(IP+2,LF,IA)
                        ELSE
                           TC = TCAL(IP,LF,LA)
                           END IF
                        IF ((VN1.EQ.FBLANK) .OR. (VN2.EQ.FBLANK) .OR.
     *                     (VN1.LE.0.0) .OR. (TC.EQ.FBLANK)) THEN
                           EDCORE(LPTR+JPTR+3*DIMIF) = FBLANK
                        ELSE
                           EDCORE(LPTR+JPTR+3*DIMIF) = VN2 / VN1/ 2.0 *
     *                        TC
                           END IF
                        VO = EDCORE(LPTR+JJPTR)
                        IF (DOCOMP) VA = EDCORE(LPTR+JPTR2)
C                                       put on TV
                        IF ((J.EQ.1) .AND. (GRSEL(4).GT.0)) THEN
                           CALL EDPLTP (XYPLOT, GRSEL(4), LT, TIME1,
     *                        TIME2, VO, 0.0, APIXR, IERR)
                           IF (IERR.GT.0) GO TO 980
                           IF ((DOCOMP) .AND. (VA.NE.FBLANK)) THEN
                              CALL EDPLTP (EXPLOT, GRSEL(4), LT, TIME1,
     *                           TIME2, VA, 0.0, APIXR2, IERR)
                              IF (IERR.GT.0) GO TO 980
                              END IF
                           END IF
                        IF ((J.GT.1) .AND. (GRSEL(6).GT.0)) THEN
                           CALL EDPLTP (XYPLOT(1,J), GRSEL(6), LT,
     *                        TIME1, TIME2, VO, 0.0, APIXR(1,J), IERR)
                           IF (IERR.GT.0) GO TO 980
                           END IF
                        END IF
                     END IF
 30               CONTINUE
               END IF
 95         CONTINUE
 100     CONTINUE
C                                       debug message
      WRITE (MSGTXT,1100) NFL, FLTIMS
      CALL MSGWRT (2)
      IF (NFL.GT.0) FLAGED = .TRUE.
      IERR = 0
      GO TO 999
C
 980  MSGTXT = 'EDSMSY: ERROR UPDATING THE TV DISPLAY FOR DATA ' //
     *   'SMOOTHING'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Altered',I6,' points in T index range',2I7)
      END
      SUBROUTINE EDSMTR (IERR)
C-----------------------------------------------------------------------
C   interactive smoothing of a time range
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLTIMS(2), TVCO(4), FLPF(2,2),
     *   TYPE
      REAL      TVXY(2), FLFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         MSGTXT = 'Hit button A to set the other side of the time range'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to smooth a time range and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to smooth a time range and return' //
     *      ' to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more data' //
     *      ' smoothing'
         CALL MSGWRT (1)
         END IF
C                                       read cursor until button
      CALL EDFLAI (1, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.GT.0) GO TO 980
C                                       smooth something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         IF (DDTYPE.NE.'SY') THEN
            TYPE = LTYPE
            IF (DDTYPE.EQ.'TY') TYPE = 1
            CALL EDSMTH (PLTAN(1), TYPE, FLTIMS, IERR)
         ELSE
            CALL EDSMSY (PLTAN(1), FLTIMS, IERR)
            END IF
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      FIRST = -1
      CALL EDFLAI (0, CORN, FIRST, TVXY, TVCO, FLTIMS, FLFLUX, FLPF,
     *   TVBUTT, IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'EDSMTR: ERROR DOING TIME-RANGE SMOOTHINGS'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE EDSTUB (SUBR, OPCODE, OBJECT)
C-----------------------------------------------------------------------
C   stub marker for edit object
C   Inputs:
C      SUBR     C*(*)   Name of subroutine
C      OPCODE   C*4     type of operation
C      OBJECT   C*(*)   object name
C-----------------------------------------------------------------------
      CHARACTER SUBR*(*), OPCODE*(*), OBJECT*(*)
C
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'You have reached the stub for ' // SUBR
      CALL MSGWRT (7)
      IF (OPCODE.NE.' ') THEN
         MSGTXT = 'The operation is of type ' // OPCODE
         CALL MSGWRT (7)
         END IF
      MSGTXT = 'On object ' // OBJECT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE EDTICS (DOTICS, IPRM, CORN, GR, PIXR, IERR)
C-----------------------------------------------------------------------
C   plot y axis tick marks and labels
C   Inputs:
C      DOTICS   I      0 return, 1, ticks, > 1 ticks and labels
C                      3 -> no metsca
C      IPRM     I      Primary or secondary parameter (= LOCNUM)
C      CORN     I(4)   TV corners of plot
C      GR       I      Graphics plane to use
C      PIXR     R(2)   Y axis data range
C   Outputs:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      INTEGER   DOTICS, IPRM, CORN(4), GR, IERR
      REAL      PIXR(2)
C
      INTEGER   DEPTH(5), IX(4), IY(2), LX, LY, I, ILEN, ITRY, FRMT,
     *   IFRMT, INOI, LLY, NTMAX
      LOGICAL   PFLG
      REAL      TMAXY, YMULT, BLC(2), TRC(2), DCX, DCY, RT
      CHARACTER SPRTXT*13
      DOUBLE PRECISION DEG, DEGC, TICX
      SAVE      YMULT
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IF (DOTICS.LE.0) GO TO 999
C                                       Label the image
      CALL FILL (5, 1, DEPTH)
      LOCNUM = IPRM
      CALL SETLOC (DEPTH, .FALSE.)
      ROT(LOCNUM) = 0.0
      LABTYP(LOCNUM) = 0
      CORTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
C                                       Y order of mag
      TMAXY = MAX (ABS(PIXR(2)), ABS(PIXR(1)))
      TMAXY = MAX (TMAXY, PIXR(2)-PIXR(1))
      IF (DOTICS.LT.3) THEN
         YMULT = TMAXY
         CALL METSCA (TMAXY, CPREF(2,LOCNUM), PFLG)
         YMULT = TMAXY / YMULT
         END IF
      RPVAL(1,LOCNUM) = TIME1
      AXINC(1,LOCNUM) = (TIME2 - TIME1) / (CORN(3) - CORN(1) - 7.)
      RPLOC(1,LOCNUM) = CORN(1) + LEDG
      RPVAL(2,LOCNUM) = PIXR(1) * YMULT
      AXINC(2,LOCNUM) = (PIXR(2) - PIXR(1)) / (CORN(4) - CORN(2) - 7.)
     *   * YMULT
      RPLOC(2,LOCNUM) = CORN(2) + LEDG
      BLC(1) = CORN(1)
      BLC(2) = CORN(2)
      TRC(1) = CORN(3)
      TRC(2) = CORN(4)
C                                       vertical
      NTMAX = (TRC(2) - BLC(2)) / (1.5 * CSIZE(2))
      CALL EDTINC (BLC, TRC, NTMAX, DEGC, DEG, INOI, TICX, ITRY, IERR)
      IF (IERR.LT.0) GO TO 999
      IF (IERR.NE.0) GO TO 980
      DCX = -0.5 * CSIZE(1)
      DCY = -0.5 * CSIZE(2)
      IX(1) = CORN(1)
      IX(2) = CORN(1) + LEDG
      IX(3) = CORN(3) - LEDG
      IX(4) = CORN(3)
C                                       Draw tic marks and values.
      LLY = 1000000
      DO 50 I = 1,INOI
         IY(1) = (DEGC - RPVAL(2,LOCNUM)) / AXINC(2,LOCNUM) +
     *      RPLOC(2,LOCNUM) + 0.5
         IY(2) = IY(1)
         CALL TVDLIN (TVNAME, 1, GR, 2, IX(1), IY, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDLIN (TVNAME, 1, GR, 2, IX(3), IY, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (DOTICS.GT.1) THEN
            RT = DEGC
            IF (ITRY.GT.9) IFRMT = FRMT (13, 3, RT, SPRTXT)
            IF (ITRY.LT.4) WRITE (SPRTXT,1270) DEGC
            IF ((ITRY.GE.4) .AND. (ITRY.LE.6)) WRITE (SPRTXT,1271) DEGC
            IF ((ITRY.GE.7) .AND. (ITRY.LE.9)) WRITE (SPRTXT,1272) DEGC
C                                       Trim blanks
            CALL CHTRIM (SPRTXT, 13, SPRTXT, ILEN)
            LX  = CORN(1) + DCX - ILEN*CSIZE(1) + 0.5
            LY = IY(1) + DCY + 0.5
            IF (LY.LE.LLY) THEN
               CALL TVDCHR (TVNAME, LX, LY, 0, 0, GR, SPRTXT(:ILEN),
     *            IERR)
               IF (IERR.NE.0) GO TO 980
               LLY = LY - CSIZE(2)
               END IF
            END IF
         DEGC = DEGC - DEG
 50      CONTINUE
      GO TO 999
C                                       Graph drawing error.
 980  MSGTXT = 'EDTICS: ERROR DRAWING Y-AXIS TICK MARKS'
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1270 FORMAT (F12.3)
 1271 FORMAT (F12.2)
 1272 FORMAT (F12.1)
      END
      SUBROUTINE EDTINC (BLC, TRC, NTMAX, DEGC, DEG, INOI, TICX, ITRY,
     *   IERR)
C-----------------------------------------------------------------------
C   figures out the tick mark lengths and increments for EDIT class
C   plots.  It is limited to vertical axes with linear coordinates.
C   Inputs:
C      BLC     R(2)     X and Y pixels to form bottom left hand
C                       corner of the graph.
C      TRC     R(2)     X and Y pixels to form the top right hand
C                       corner of the graph.
C      NTMAX   I        Maximum number of ticks allowed
C   Outputs:
C      DEGC    D        Value at first tick
C      DEG     D        Tick increment
C      INOI    I        Number of ticks
C      TICX    D        Tick length in x units
C      ITRY    I        subscript in coordinate incr array
C      IERR    I        error code: 0 => ok
C                                   3 => tic algorithm fails
C-----------------------------------------------------------------------
      DOUBLE PRECISION DEGC, DEG, TICX
      REAL      BLC(2), TRC(2)
      INTEGER   NTMAX, INOI, ITRY, IERR
C
      DOUBLE PRECISION DEGL, DEGU, DBX, DIFF, DTX, DX, DBY, DTY, DY, DZ,
     *   XINTER(27), DTZ
      REAL      PXT, PYT, X, Y
      INTEGER   IXT, IXTNO, ISGN, JERR, JJ
      LOGICAL   TRUBLE
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
C      INCLUDE 'INCS:DGPH.INC'
C
      DATA XINTER /0.001D0,0.002D0,0.005D0,0.01D0,0.02D0,0.05D0,0.1D0,
     *   0.2D0,0.5D0,1.D0,2.D0,5.D0,10.D0,20.D0,50.D0,100.D0,200.D0,
     *   500.D0,1000.D0,2000.D0,5000.D0,10000.D0,20000.D0,50000.D0,
     *   100000.D0,200000.D0,500000.D0/
C-----------------------------------------------------------------------
      CALL CHECKL ('TICINC')
      TRUBLE = .FALSE.
C                                       Find value at BLC.
C                                       Determine axis type.
C                                       vertical
      CALL XYVAL (BLC(1), BLC(2), DBX, DBY, DZ, IERR)
      IF (IERR.EQ.0) CALL XYVAL (BLC(1), TRC(2), DX, DY, DZ, IERR)
      IF (IERR.NE.0) THEN
         PXT = (BLC(1) + TRC(1)) / 2.0
         CALL XYVAL (PXT, BLC(2), DBX, DBY, DZ, IERR)
         IF (IERR.EQ.0) CALL XYVAL (PXT, TRC(2), DX, DY, DZ, IERR)
         IF (IERR.NE.0) THEN
            PYT = (BLC(2) + TRC(2)) / 2.0
            CALL XYVAL (PXT, PYT, DX, DY, DZ, IERR)
            IF (IERR.NE.0) GO TO 970
            PYT = PYT + (TRC(2) - BLC(2)) / 10.0
            CALL XYVAL (PXT, PYT, DTX, DTY, DTZ, IERR)
            IF (IERR.NE.0) GO TO 970
            DBY = DY - 5.0D0 * (DTY - DY)
            DY = DY + 5.0D0 * (DTY - DY)
            TRUBLE = .TRUE.
            END IF
         END IF
      TICX = ABS ((TRC(1)-BLC(1) + (TRC(2)-BLC(2))) / 80.0)
      IF (TICX.LE.0.05) TICX = 3 * TICX
      TICX = ABS (AXINC(1,LOCNUM) * TICX)
      DEGU = DY
      DEGL = DBY
      ISGN = SIGN (1.0, AXINC(2,LOCNUM))
      IXTNO = NTMAX
      IXTNO = MIN (11, MAX (2, IXTNO))
C                                       Try different values.
      IXT = 2
 125  DO 130 ITRY = 1,27
         DEG = ISGN * XINTER(ITRY)
         DEGC = DINT(DEGU/DEG) * DEG
         IF (ISGN * DEGU.LT.0.0) DEGC = DEGC - DEG
         DIFF = ISGN * (DEGC - DEGL)
         IF (ABS(DIFF / XINTER(ITRY)).GT.32000.0) GO TO 130
            INOI = (DIFF / XINTER(ITRY)) + 1
            IF ((INOI.GT.IXT) .AND. (INOI.LE.IXTNO)) GO TO 140
 130     CONTINUE
      IF (IXT.NE.0) THEN
         IXT = 0
         GO TO 125
C                                       Tic mark algorithm failed.
      ELSE
         WRITE (MSGTXT,1130)
         CALL MSGWRT (6)
         IERR = -1
         GO TO 999
         END IF
C                                       check number of actual ticks
 140  JJ = ABS (MIN (DEGL,DEGU)) / XINTER(ITRY)
      IXT = ABS (MAX (DEGL,DEGU)) / XINTER(ITRY)
      IF (MIN(DEGL,DEGU).GT.0) THEN
         JJ = JJ + 1
      ELSE
         JJ = -JJ
         END IF
      IF (MAX(DEGL,DEGU).LT.0) IXT = -IXT - 1
      INOI = IXT - JJ + 1
      IF ((INOI.LE.1) .AND. (ITRY.GT.1)) THEN
         ITRY = ITRY - 1
         GO TO 140
         END IF
      DEG = ISGN * XINTER(ITRY)
      DEGC = DINT(DEGU/DEG) * DEG
      IF (ISGN * DEGU.LT.0.0) DEGC = DEGC - DEG
C                                       extend ? when bail out comp.
      IF (TRUBLE) THEN
 150     DY = DEGC + DEG
            DX = DY
            CALL FNDX (BLC(1), DY, DX, JERR)
            IF (JERR.NE.0) GO TO 160
            CALL XYPIX (DX, DY, X, Y, JERR)
            IF (JERR.NE.0) GO TO 160
            IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 160
            IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 160
               DEGC = DEGC + DEG
               INOI = INOI + 1
               GO TO 150
 160     DY = DEGC - INOI * DEG
            DX = DY
            CALL FNDX (BLC(1), DY, DX, JERR)
            IF (JERR.NE.0) GO TO 170
            CALL XYPIX (DX, DY, X, Y, JERR)
            IF (JERR.NE.0) GO TO 170
            IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 170
            IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 170
               INOI = INOI + 1
               GO TO 160
C                                       change increment?
 170     IF (INOI.GT.IXTNO) THEN
            DEGU = DEGC
            DEGL = DEGC - DEG * (INOI-1)
            ITRY = ITRY + 1
            DEG = ISGN * XINTER(ITRY)
            DEGC = DINT(DEGU/DEG) * DEG
            IF (ISGN * DEGU.LT.0.0) DEGC = DEGC - DEG
            DIFF = ISGN * (DEGC - DEGL)
            INOI = (DIFF / XINTER(ITRY)) + 1
            END IF
         END IF
      GO TO 999
C                                       Total position failure
 970  IERR = 3
      WRITE (MSGTXT,1970)
      CALL MSGWRT (7)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1130 FORMAT ('EDTINC: TIC MARK ALGORITHM FAILED! CONTINUING.')
 1970 FORMAT ('EDTINC: POSITION ROUTINES FAIL ON THIS IMAGE')
      END
      SUBROUTINE EDTIMI (OT, IT)
C-----------------------------------------------------------------------
C   Find index range interior to a specified time range
C   Inputs:
C      OT   R(2)   Extended time range (days)
C   Output
C      IT   I(2)   Time range - as indices into TIMES array
C   Depends on AP commons etc.
C-----------------------------------------------------------------------
      INTEGER   IT(2)
      REAL      OT(2)
C
      INTEGER   I
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
C                                       lower limit
      IF (OT(1).LE.TIMES(EDIPTR+1)) THEN
         IT(1) = 1
      ELSE IF (OT(1).GE.TIMES(EDIPTR+TIMEM)) THEN
         IT(1) = TIMEM
      ELSE
         DO 10 I = 1,TIMEM
            IF (OT(1).LE.TIMES(EDIPTR+I)) THEN
               IT(1) = I
               GO TO 20
               END IF
 10         CONTINUE
         END IF
C                                       upper limit
 20   IF (OT(2).LE.TIMES(EDIPTR+1)) THEN
         IT(2) = 1
      ELSE IF (OT(2).GE.TIMES(EDIPTR+TIMEM)) THEN
         IT(2) = TIMEM
      ELSE
         DO 30 I = 1,TIMEM
            IF (OT(2).LE.TIMES(EDIPTR+I)) THEN
               IT(2) = MAX (1, I-1)
               GO TO 999
               END IF
 30         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDTIMX (IT, OT)
C-----------------------------------------------------------------------
C   extends a time range to half way closer to the next time outward
C   Inputs:
C      IT   I(2)   Time range - as indices into TIMES array
C   Output
C      OT   R(2)   Extended time range (days)
C   Depends on AP commons etc.
C-----------------------------------------------------------------------
      INTEGER   IT(2)
      REAL      OT(2)
C
      REAL      TEPS, DT1, DT2, D
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      DATA TEPS /2.7E-7/
C-----------------------------------------------------------------------
C                                       lower limit
      DT1 = -1.0
      IF (IT(1).LE.1) THEN
         OT(1) = TIMES(EDIPTR+1)
      ELSE IF (IT(1).GE.TIMEM) THEN
         OT(1) = TIMES(EDIPTR+TIMEM)
      ELSE IF (TIMES(EDIPTR+IT(1)-1).GE.TIMES(EDIPTR+IT(1))-TEPS) THEN
         DT1 = (TIMES(EDIPTR+IT(1)+1) - TIMES(EDIPTR+IT(1))) / 2.0
C         OT(1) = (3.*TIMES(EDIPTR+IT(1)) - TIMES(EDIPTR+IT(1)+1)) / 2.0
      ELSE
         DT1 = (TIMES(EDIPTR+IT(1)) - TIMES(EDIPTR+IT(1)-1)) / 2.0
C         OT(1) = (TIMES(EDIPTR+IT(1)-1) + TIMES(EDIPTR+IT(1))) / 2.0
         END IF
C                                       upper limit
      DT2 = -1.0
      IF (IT(2).LE.1) THEN
         OT(2) = TIMES(EDIPTR+1)
      ELSE IF (IT(2).GE.TIMEM) THEN
         OT(2) = TIMES(EDIPTR+TIMEM)
      ELSE IF (TIMES(EDIPTR+IT(2)+1)-TEPS.LE.TIMES(EDIPTR+IT(2))) THEN
         DT2 = (TIMES(EDIPTR+IT(1)) - TIMES(EDIPTR+IT(1)-1)) / 2.0
C         OT(2) = (3.*TIMES(EDIPTR+IT(2)) - TIMES(EDIPTR+IT(2)-1)) / 2.0
      ELSE
         DT2 = (TIMES(EDIPTR+IT(1)+1) - TIMES(EDIPTR+IT(1))) / 2.0
C         OT(2) = (TIMES(EDIPTR+IT(2)+1) + TIMES(EDIPTR+IT(2))) / 2.0
         END IF
      IF ((DT1.GT.0.0) .AND. (DT2.GT.0.0)) THEN
         D = MIN (DT1, DT2)
         OT(1) = TIMES(EDIPTR+IT(1)) - D
         OT(2) = TIMES(EDIPTR+IT(2)) + D
      ELSE IF (DT1.GT.0.0) THEN
         OT(1) = TIMES(EDIPTR+IT(1)) - DT1
      ELSE IF (DT2.GT.0.0) THEN
         OT(2) = TIMES(EDIPTR+IT(2)) + DT2
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDSORL (IT, NS, SL)
C-----------------------------------------------------------------------
C   finds a list of source numbers in time range IT
C   Inputs:
C      IT   I(2)   Time range as indices into TIMES array
C   In/out
C      NS   I      in: size of SL
C                  out: Number sources in SL
C   Outputs:
C      SL   I(*)   List of source numbers
C-----------------------------------------------------------------------
      INTEGER   IT(2), NS, SL(*)
C
      INTEGER   MS, I, J, K
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
      MS = MAX (1, NS)
      NS = 0
      SL(1) = 0
      DO 20 I = IT(1),IT(2)
         J = MAX (1, MIN (TIMEM, I))
         J = EDCORI(J+SUPTR-1)
         IF (J.GT.0) THEN
            DO 10 K = 1,NS
               IF (J.EQ.SL(K)) GO TO 20
 10            CONTINUE
            NS = NS + 1
            SL(NS) = J
            IF (NS.GE.MS) GO TO 999
            END IF
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE EDTWIN (OPCODE, TYPE, IERR)
C-----------------------------------------------------------------------
C   Plots/erases the window (frame) marks
C   Inputs:
C      OPCODE   C*(*)    ON or OFF
C      TYPE     I        1 => left, 2 => right, 0 => both
C   Output:
C      IERR     I        Error code
C-----------------------------------------------------------------------
      CHARACTER OPCODE*(*)
      INTEGER   TYPE, IERR
C
      INTEGER   IX(2), IY(2), PT
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      IY(1) = XYPLOT(2,1) + 1
      IY(2) = XYPLOT(4,NUMPLT) - 1
      PT = 3
      IF (OPCODE(:2).EQ.'ON') THEN
         PT = 1
         IF (TYPE.EQ.0) THEN
            CALL TVDOPR (TVNAME, 'GRCL', GRSEL(3), IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       left
      IF ((TYPE.EQ.0) .OR. (TYPE.EQ.1)) THEN
         IX(1) = (TIME1 - 1.0) / (TIMEM - 1.0) * (XYPLOT(3,1) -
     *      XYPLOT(1,1) - 2*LEDG) + 0.5 + XYPLOT(1,1) + LEDG
         IX(2) = IX(1)
         CALL TVDLIN (TVNAME, PT, GRSEL(3), 2, IX, IY, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       right
      IF ((TYPE.EQ.0) .OR. (TYPE.EQ.2)) THEN
         IX(1) = (TIME2 - 1.0) / (TIMEM - 1.0) * (XYPLOT(3,1) -
     *      XYPLOT(1,1) - 2*LEDG) + 0.5 + XYPLOT(1,1) + LEDG
         IX(2) = IX(1)
         CALL TVDLIN (TVNAME, PT, GRSEL(3), 2, IX, IY, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      GO TO 999
C
 980  MSGTXT = 'EDTWIN: ERROR PLOTING TOP PLOT''S WINDOW'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE EDWINC (XMIN, YMIN, IERR)
C-----------------------------------------------------------------------
C   reads current TV parameters, forces TV size to be big enough
C   sets TOPLOT = top of uppermost plot
C   Inputs:
C      XMIN     I   Min X pixels
C      YMIN     I   Min Y pixels, excluding top plot
C   Outputs:
C      IERR     I   Error code from TV IO
C-----------------------------------------------------------------------
      INTEGER   XMIN, YMIN, IERR
C
      LOGICAL   DOIT
      INTEGER   TVWND(4), I, NTRY, HCS
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
C                                       read TV parameters
      CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
      IF (IERR.NE.0) GO TO 999
      DOIT = .FALSE.
      I = XMIN - TVWND(3) + TVWND(1)
      IF (I.GT.0) THEN
         I = (I + 1) / 2
         TVWND(1) = TVWND(1) - I
         TVWND(3) = TVWND(3) + I
         TVWND(1) = MAX (1, TVWND(1))
         TVWND(3) = MIN (MAXX(1), TVWND(3))
         DOIT = .TRUE.
         END IF
      NTRY = 0
      HCS = (CSIZE(2) + 1) / 2
C                                       loop to do Y size
 10   TOPLOT = TVWND(4) - LEDG - 2 * HCS - CSIZE(2)
      I = TVWND(4) - TOPLOT + CSIZE(2) - 1
      I = YMIN + I - TVWND(4) + TVWND(2)
      IF (I.GT.0) THEN
         I = (I + 1) / 2
         TVWND(2) = TVWND(2) - I
         TVWND(4) = TVWND(4) + I
         TVWND(2) = MAX (1, TVWND(2))
         TVWND(4) = MIN (MAXX(2), TVWND(4))
         DOIT = .TRUE.
         NTRY = NTRY + 1
         IF (NTRY.LE.5) GO TO 10
         END IF
C                                       reset screen size
      IF (DOIT) THEN
         CALL TVDTVW (TVNAME, 'WRITE', TVWND, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      NEWPLT = (TVWND(1).NE.LTVWND(1)) .OR. (TVWND(2).NE.LTVWND(2))
     *   .OR. (TVWND(3).NE.LTVWND(3)) .OR. (TVWND(4).NE.LTVWND(4))
     *   .OR. NEWPLT
      CALL COPY (4, TVWND, LTVWND)
      TOPLOT = LTVWND(4) - LEDG - 2 * HCS - CSIZE(2)
C
 999  RETURN
      END
