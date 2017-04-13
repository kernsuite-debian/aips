C   Edit Class utility module (does all the real editing)
C-----------------------------------------------------------------------
C! UV data editing with the TV and bandpass-like tables
C# Task AP UV EDITING TV-APPL CALIBRATION
C-----------------------------------------------------------------------
C;  Copyright (C) 2016
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
C   EDITBP (opcode, object, tbltyp, ierr)
C      edit a UV data object with a tbltyp table object
C   EDITPC (opcode, object, tbltyp, ierr)
C      edit a PC table
C-----------------------------------------------------------------------
C   Private functions:
C
C   BPAFND (ant, antens, ierr)
C      find antenna number in list of desired antennas
C   BPANTS (ants, ant1, ant2)
C      Breaks 256*m + n into its parts
C   BPDBOX (name, type, mode, chan, corn, ierr)
C      draw a box on the TV
C   BPFCAP (uvdata, ierr)
C      Applies an FC table to a uv data set (FG file)
C   PCFCAP (uvdata, ierr)
C      Applies an FC table to a PC table
C   BPFCDO (ierr)
C      Reapplies FC table's contents to data in core
C   BPFCLI (ierr)
C      lists an FC table's contents
C   BPFCUN (entry, ierr)
C      Undoes one entry in the FC table
C   BPFGAP (uvdata, ierr)
C      Applies a uv data set FG table to table data now in core
C   BPFIND (mode, wtype, ian, type, tvxy, x, jtch, ir, ierr)
C      find the nearest plotted sample to the TV cursor in TVXY
C   BPFLAG (op, ian, type, flgtim, flgflx, nfl, ierr)
C      flag samples in specified range, remove them from plot and
C      toplot, put them on flagged plot (if any)
C   BPFLAI (type, corn, first, tvxy, tvco, flchns, flgflx, tvbutt, ierr)
C      interactive display of cursor position to set a flagging box
C   BPFLAR (ierr)
C      interactive flagging of an area: time range and flux range
C   BPFLFA (ierr)
C      interactive flagging of high fluxes
C   BPFLFB (ierr)
C      interactive flagging of low fluxes
C   BPFLPT (ierr)
C      interactive flagging of points (uses Y and X position)
C   BPFLQU (ierr)
C      interactive flagging of points in quick mode
C   BPFLCI (ierr)
C      interactive flagging of a single channel
C   BPFLCR (ierr)
C      interactive flagging of a time-range
C   BPFRAM (ierr)
C      interactive setting of display frame
C   BPGTAP (ant, type, flchns, flflux, ierr)
C      get the next sample in the flchms, flflux area even if flagged
C   BPGTBP (ierr)
C      Gets BP data from OBJECT for specified IF
C   PCGTPC (ierr)
C      Gets PC data from OBJECT for specified IF
C   BPMXMN (ian, type, pixr, ierr)
C      find max and min of specified data type and antenna/baseline
C   BPMXMT (ian, type, pixr, ierr)
C      if fixed phase plot range, try to force data into range
C   BPOANT (all, ian, lan)
C      determines if the current antenna/baseline is included
C   BPPLOT (ierr)
C      plot the data on the TV
C   BPPLSS (gr, ierr)
C      plot status strings from lower left corner of visible area
C   BPPLST (gr, ierr)
C      plot status strings from lower left corner of visible area
C      (calls BPPLSS)
C   BPPLT1 (ian, iprm, corn, gr1, gr2, gr3, it1, it2, dotics, lpixr,
C      ierr)
C      Plots one antenna worth of data
C   BPPLTP (corn, gr, lf, lf1, lf2, v, lpixr, ierr)
C      plots one point
C   BPSLST (ierr)
C      fills in the SLIST common variable with a list of source names if
C      there is a source table attached to the object or makes a default
C      list.
C   BPSORL (fltims, nsor, sorl)
C      returns list of source numbers in a time range
C   BPSTUB (subr, opcode, object)
C      stub point reporter
C   BPTICS (dotics, iprm, corn, gr, pixr, ierr)
C      plot y axis tick marks and labels
C   BPTINC (blc, trc, ntmax, degc, deg, inoi, ticx, itry, ierr)
C      figures out the tick mark lengths and increments for EDIT class
C      plots.  It is limited to vertical axes with linear coordinates.
C   BPTIMI (ot, it)
C      converts extended time range to index renage just interior
C   BPTIMX (it, ot)
C      extends a time range to half way closer to the next time outward
C   BPTWIN (opcode, type, ierr)
C      plots/erases the window marks on the top plot
C   BPWINC (xmin, ymin, ierr)
C      reads current TV parameters, forces TV size to be big enough
C-----------------------------------------------------------------------
C    memory allocation:
C      Begin   Number     What
C          1    TIMEM     Times in floating days
C       TIPTR   TIMEM     Time intervals in days
C       SUPTR   TIMEM     Source numbers
C       PPTR    TIMEM     Record number of first data sample at time i
C                         as addresses in data area (EDCORE(DPTR++))
C       DPTR   MAXREC     Data records each DIMREC words long
C                            DIMPRM + Array(1-DIMIF, 1-DIMDAT, 1-POLMAX)
C                            DIMPRM = antenna, time counter, table row#,
C                            DIMIF = EIF-BIF+1  or 1 depending on core
C                            DIMDAT = (wt amp phase resid freqs)*CHNMAX
C                            POLMAX = 1 or 2
C                                  resid, freqs only for PC table
C-----------------------------------------------------------------------
LOCAL INCLUDE 'EDIUTIL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LEDG, CEDG, PINC, NSRC, NPLT
      PARAMETER (LEDG = 4)
      PARAMETER (CEDG = 5)
      PARAMETER (PINC = 2)
      PARAMETER (NSRC = 10000)
      PARAMETER (NPLT = 11)
C
      CHARACTER UVMAST*32, TBEDIT*32, TVNAME*32, DTYPE(3)*12, DDSTR*64,
     *   DDTYPE*2, COLLAB(15)*24, FCFILE*32, SLIST(NSRC)*16,
     *   REASON*24, DUNITS(3)*8
      LOGICAL   NEWPLT, ALLPOL, PNDING, UVFLAG, FLGMSG, FLAGED, FCREAT,
     *   APOPEN, ALLTIM, DOCOMP, ALLSOR, PREXIS
      LONGINT   DPTR, PPTR, SUPTR, SPTR, TIPTR
      INTEGER   BIF, EIF, ANTEN(MAXANT), ANTMAX, POLMAX, IFMAX, IFNOW,
     *   POLNOW, GRSEL(10), ROWMAX, LTVWND(4), LTYPE, LTYPE2, NUMPLT,
     *   PLTAN(NPLT), PIFNOW, EXPLOT(4), XYPLOT(4,NPLT), CSIZE(2),
     *   CHAN1, CHAN2, TIMEC, TIMEM, ANTMLX, NGRY, NGRPH, MAXX(2),
     *   DIMIF, DIMDAT, DIMREC, MAXREC, FRQSEL, DDSLEN, WASFLG, DIMPRM,
     *   COLNUM(15), SUBARR, ALLANT, ANTNOW, PANTNW, TOPLOT, NUMVAL,
     *   PLTPOL, FLGNMX, MSAMPS(MAXANT), SUNUMB, CHNMAX, CHNTOT, TIMEL
      REAL      TSTART, TEND, DPIXR(2,8), APIXR(2,NPLT), DTIME, GTIME,
     *   RGB8(3), DPLSCL(8), APIXR2(2), WTSCAL, TCAL(2,MAXIF,MAXANT),
     *   RSCALE(2)
      COMMON /EDITCM/ DPTR, PPTR, SUPTR, TIPTR, SPTR, BIF, EIF,
     *   ANTEN, ANTMAX, POLMAX, IFMAX, TSTART, TEND, IFNOW, POLNOW,
     *   GRSEL, ROWMAX, LTVWND, LTYPE, NUMPLT, PLTAN, PIFNOW, WASFLG,
     *   DPIXR, EXPLOT, XYPLOT, CSIZE, CHAN1, CHAN2, TIMEM, APIXR,
     *   ANTMLX, NEWPLT, NGRY, NGRPH, MAXX, DIMPRM, DIMIF, DIMDAT,
     *   DIMREC, MAXREC, DTIME, GTIME, FRQSEL, DDSLEN, ALLANT, ALLPOL,
     *   COLNUM, PNDING, SUBARR, UVFLAG, FLGMSG, RGB8, FLAGED, ANTNOW,
     *   PANTNW, FCREAT, APOPEN, ALLTIM, DPLSCL,DOCOMP, LTYPE2, TOPLOT,
     *   APIXR2, WTSCAL, PLTPOL, ALLSOR, FLGNMX, MSAMPS, SUNUMB, PREXIS,
     *   TCAL, RSCALE, CHNMAX, CHNTOT, TIMEC, TIMEL, NUMVAL
      COMMON /EDITCH/ UVMAST, TBEDIT, TVNAME, DTYPE, DDSTR, DDTYPE,
     *   COLLAB, FCFILE, SLIST, REASON, DUNITS
      INCLUDE 'INCS:DMSG.INC'
LOCAL END
LOCAL INCLUDE 'EDIUTAP.INC'
      LONGINT   EDIPTR
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   EDCORI(2), EDSIZE
      REAL      TIMES(2), EDCORE(2)
C      INTEGER   EDCORI(200000), EDSIZE
C      REAL      TIMES(2), EDCORE(200000)
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
      SUBROUTINE EDITBP (OPCODE, OBJECT, TBLTYP, IERR)
C-----------------------------------------------------------------------
C   edit a UV data object with an BP type table object
C   Inputs:
C      OPCODE   C*4    Operation:
C                         'INIT' start a new edit session
C                         'ABOR' close session, delete output table
C                         'KILL' close session, save FC table
C                         'APPL' close session, save output table
C      OBJECT   C*(*)  Open Edit object
C      TBLTYP   C*2    Table type
C   Output:
C      IERR     I      Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C                         < 0 => dies by the users' hands
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OBJECT*(*), TBLTYP*2
      INTEGER   IERR
C
      INTEGER   NCOL1, NCOL2, NCOLS
      PARAMETER (NCOL1 = 20, NCOL2 = 18, NCOLS = NCOL1+NCOL2)
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'EDIFCPS.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER   TYPE, DIM(7), SCRTCH(256), I, J, K, TVCOL, TVROWS(2),
     *   TIMLIM, CHS, TVWND(4), MINWIN(2), VERS, IG(8), BUTTON, DUMMY,
     *   TTY(2), ITRIM, IVAL(NPLT), NOZOOM(3), SVZOOM(3), ILAST, LT1,
     *   LT2, TVSTAT(16), GRSTAT(8), JERR, IFLN(2), CHSHOW, CHSHO2,
     *   MSGSAV, AVAL(50), NTITLE, SIDSEP
      REAL      T(8)
      DOUBLE PRECISION DVAL(2), UVFREQ
      LOGICAL   BPAFND, DOPLOT, LOADIT, LEAVE(NCOLS), WANT
      CHARACTER STATUS*4, PROBLM*32, CDUMMY*1, CHOICS(NCOLS)*16,
     *   ISHELP*6, LIST2(NCOL2)*16, MSGBUF*72, DDNAME*18, INKT(4)*8,
     *   INEXT*2, LIST1(NCOL1)*16, REAZON*24, CHOICE*16, TITLE*8
      DATA INKT /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
      DATA LIST1 /'FLAG CHANNEL', 'FLAG CHAN RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'ENTER AMPL RNG', 'ENTER PHASE RNG', 'LIST FLAGS',
     *   'UNDO FLAGS', 'REDO FLAGS', 'SET REASON', 'TV ZOOM',
     *   'OFF ZOOM', 'HOLD TV LOAD', 'REPLOT',' ', 'EXIT', 'ABORT'/
      DATA LIST2 /'SWITCH POLARIZ', 'SWITCH ALL POL',
     *   'SWITCH ALL TIME', 'SWITCH ALL SOURC', 'SWITCH ALL ANT',
     *   'ENTER ANTENNA', 'ENTER OTHER ANT', 'NEXT ANTENNA',
     *   'PLOT ALL CHANNEL', 'SELECT FRAME', 'NEXT FRAME',
     *   'PREVIOUS FRAME', 'SELECT IF', 'NEXT IF', 'LAST IF',
     *   'NEXT TIME', 'LAST TIME', 'SHOW PHASE'/
      DATA LEAVE /NCOLS*.TRUE./
C-----------------------------------------------------------------------
      TTY(1) = 5
      TTY(2) = 0
      ALLSOR = .FALSE.
C                                       plot scale ranges
      CALL RFILL (2, 1.0, RSCALE)
C                                       Init the functions
      IF (OPCODE.EQ.'INIT') THEN
         TIMEC = 2
         TIMEL = 0
         FLAGED = .TRUE.
         APOPEN = .FALSE.
         FCREAT = .FALSE.
         FLGMSG = .TRUE.
         FCFILE = ' '
         NEWPLT = .TRUE.
         ILAST = 0
         DTYPE(1) = 'Amplitude'
         DTYPE(2) = 'Phase'
         DUNITS(1) = 'Gains'
         DUNITS(2) = 'Degrees'
         CALL RFILL (2, 1.0, DPLSCL)
         ALLPOL = .FALSE.
         ALLTIM = .FALSE.
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
C                                       get UV data frequency
         PROBLM = UVMAST
         CALL OOPEN (UVMAST, 'READ', IERR)
         IF (IERR.NE.0) GO TO 980
         CALL UVDGET (UVMAST, 'REFFREQ', TYPE, DIM, UVFREQ, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL OCLOSE (UVMAST, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Make sure that the table is
C                                       sorted correctly
         CALL TBLSRT (TBEDIT, 'TIME', 'ANTENNA', IERR)
         IF (IERR.NE.0) GO TO 999
         PROBLM = TBEDIT
C                                       Open table object: get adverbs
         STATUS = 'READ'
         CALL TABOPN (TBEDIT, STATUS, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Get a source list
         CALL BPSLST (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       FC table for UV flagging
         FCFILE = 'Flag Command table for BP edits'
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
            MSGTXT = '**** WARNING: USING A PRE-EXISTING FC TABLE' //
     *         ' ****'
            CALL MSGWRT (6)
            END IF
         CALL OFCINI (FCFILE, 'WRIT', FLGNUM, FCROW, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *      FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.NE.0) GO TO 980
         FLGNMX = FLGNUM
C                                       get other adverbs
         PROBLM = TBEDIT
         MSGSAV = MSGSUP
         MSGSUP = 32000
         DOCOMP = .TRUE.
         MSGSUP = 32000
         CALL OUVGET (TBEDIT, 'REASON', TYPE, DIM, DUMMY, REASON, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            REASON = ' '
            IERR = 0
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
         DTIME = 1 / 6000.0
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0 * DTIME)
         CALL TABGET (TBEDIT, 'ANTENNAS', TYPE, DIM, SCRTCH, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
C                                       parameters of BP
         CALL OGET (TBEDIT, 'NROW', TYPE, DIM, ROWMAX, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IFMAX = EIF - BIF + 1
         CALL OGET (TBEDIT, 'KEY.NO_CHAN', TYPE, DIM, CHNMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (CHNMAX.LE.0) CHNMAX = 1
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
         CHAN1 = 1
         CHAN2 = 0
         PIFNOW = -1
         MAXREC = 0
         POLNOW = 1
         LTYPE = 1
         LTYPE2 = 0
         IF (DOCOMP) LTYPE2 = 2
         IFNOW = BIF
C                                       set default ranges to full range
C                                       of phases, -1000:1000 ns & mHz
C                                       self scale in non-expert
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
         CALL BPWINC (MINWIN(1), MINWIN(2), IERR)
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
         CALL TVDOPR (TVNAME, 'HOFF', I, IERR)
         IF (IERR.NE.0) GO TO 980

C                                       Get some data
         WASFLG = -1
         PROBLM = TBEDIT
         CALL BPGTBP (IERR)
         IF (IERR.NE.0) GO TO 980
         IF (POLMAX.LE.1) THEN
            POLNOW = 1
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
     *         (BPAFND(AVAL(I),ANTMLX,ANTEN)) .AND.
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
         CALL BPPLOT (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       set parameters for menu
         TVCOL = 2
         TVROWS(1) = NCOL1
         DO 50 I = 1,NCOL1
            CHOICS(I) = LIST1(I)
 50         CONTINUE
         J = TVROWS(1)
         IF (POLMAX.GT.1) THEN
            J = J + 1
            CHOICS(J) = LIST2(1)
            J = J + 1
            CHOICS(J) = LIST2(2)
            END IF
         IF (EIF.GT.BIF) THEN
            J = J + 1
            CHOICS(J) = LIST2(3)
            END IF
         CHSHO2 = 0
         DO 60 I = 4,NCOL2
            J = J + 1
            CHOICS(J) = LIST2(I)
            IF (CHOICS(J).EQ.'SHOW PHASE') CHSHOW = J
 60         CONTINUE
         TVROWS(2) = J - TVROWS(1)
         IF (TIMEM.LE.1) TVROWS(2) = TVROWS(2) - 2
         ISHELP = 'EDIBP'
         TIMLIM = 0
         IG(1) = GRSEL(1)
         IG(2) = GRSEL(2)
C                                       LOOP POINT
C                                       window still big enough?
 100     PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL BPWINC (MINWIN(1), MINWIN(2), IERR)
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
         CALL BPWINC (MINWIN(1), MINWIN(2), IERR)
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
               CALL BPPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (CHOICE.EQ.'FLAG CHANNEL') THEN
               CALL BPFLCI (IERR)
            ELSE IF (CHOICE.EQ.'FLAG CHAN RANGE') THEN
               CALL BPFLCR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG BELOW') THEN
               CALL BPFLFB (IERR)
            ELSE IF (CHOICE.EQ.'FLAG ABOVE') THEN
               CALL BPFLFA (IERR)
            ELSE IF (CHOICE.EQ.'FLAG AREA') THEN
               CALL BPFLAR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG POINT') THEN
               CALL BPFLPT (IERR)
            ELSE IF (CHOICE.EQ.'FLAG QUICKLY') THEN
               CALL BPFLQU (IERR)
            ELSE
               CALL BPSTUB ('FLAG', CHOICE, OBJECT)
               END IF
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       list FC table
         ELSE IF (CHOICE.EQ.'LIST FLAGS') THEN
            CALL BPFCLI (IERR)
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
               CALL BPFCUN (IFLN, IERR)
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Redo flags in FC table
         ELSE IF (CHOICE.EQ.'REDO FLAGS') THEN
            CALL BPFCDO (IERR)
            PREXIS = .FALSE.
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
            ELSE IF ((BPAFND (I, ANTMLX, ANTEN)) .AND. (MSAMPS(I).GT.0))
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
C                                       next time
         ELSE IF (CHOICE.EQ.'NEXT TIME') THEN
            TIMEC = TIMEC + 1
            IF (TIMEC.GT.TIMEM-2) TIMEC = 2
            DOPLOT = TIMEC.NE.TIMEL
            PNDING = DOPLOT
C                                       next time
         ELSE IF (CHOICE.EQ.'LAST TIME') THEN
            TIMEC = TIMEC - 1
            IF (TIMEC.LT.2) TIMEC = TIMEM - 2
            DOPLOT = TIMEC.NE.TIMEL
            PNDING = DOPLOT
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'NEXT ANTENNA') THEN
            I = PLTAN(1)
 110        I = I + 1
               IF (I.GT.ANTMAX) I = 1
               IF (MSAMPS(I).LE.0) GO TO 110
               IF (.NOT.(BPAFND (I, ANTMLX, ANTEN))) THEN
                  MSGTXT = 'BPAFND FAILS'
                  CALL MSGWRT (3)
                  GO TO 110
                  END IF
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
                     IF ((BPAFND (I, ANTMLX, ANTEN)) .AND.
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
                     IF ((BPAFND (IVAL(I), ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(IVAL(I)).GT.0)) THEN
                        NUMPLT = NUMPLT + 1
                        PLTAN(NUMPLT) = IVAL(I)
                        END IF
                     END IF
 210              CONTINUE
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               IF (K.NE.NUMPLT) THEN
                  CALL BPWINC (MINWIN(1), MINWIN(2), IERR)
                  IF (IERR.NE.0) GO TO 980
                  NEWPLT = .TRUE.
                  ILAST = 0
                  END IF
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
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
               CALL BPSTUB ('ENTER', CHOICE, OBJECT)
               END IF
C                                       Switch polarization
         ELSE IF (CHOICE.EQ.'SWITCH POLARIZ') THEN
            POLNOW = 3 - POLNOW
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Switch all ant
         ELSE IF (CHOICE.EQ.'SWITCH ALL ANT') THEN
            IF (ALLANT.GT.0) THEN
               ALLANT = 0
            ELSE
               ALLANT = PLTAN(1)
               END IF
            CALL BPPLST (GRSEL(6), IERR)
C                                       Switch all pol
         ELSE IF (CHOICE.EQ.'SWITCH ALL POL') THEN
            IF (POLMAX.GT.1) THEN
               ALLPOL = .NOT.ALLPOL
               CALL BPPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all pol
         ELSE IF (CHOICE.EQ.'SWITCH ALL TIME') THEN
            ALLTIM = .NOT.ALLTIM
            CALL BPPLST (GRSEL(6), IERR)
C                                       Switch all source
         ELSE IF (CHOICE.EQ.'SWITCH ALL SOURC') THEN
            ALLSOR = .NOT.ALLSOR
            CALL BPPLST (GRSEL(6), IERR)
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
C                                       full plot
         ELSE IF (CHOICE.EQ.'PLOT ALL CHANNEL') THEN
            IF ((CHAN1.GT.1) .OR. (CHAN2.LT.CHNTOT)) THEN
               CHAN1 = 1
               CHAN2 = CHNTOT
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Select frame
         ELSE IF (CHOICE.EQ.'SELECT FRAME') THEN
            IF ((CHAN1.GT.1) .OR. (CHAN2.LT.CHNTOT) .OR. PNDING) THEN
               CHAN1 = 1
               CHAN2 = CHNTOT
               CALL BPPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL BPFRAM (IERR)
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Next frame
         ELSE IF (CHOICE.EQ.'NEXT FRAME') THEN
            LT2 = MIN (2*CHAN2-CHAN1, CHNTOT)
            LT1 = LT2 - CHAN2 + CHAN1
            IF ((CHAN1.NE.LT1) .OR. (CHAN2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               CHAN1 = LT1
               CHAN2 = LT2
               END IF
C                                       Previous frame
         ELSE IF (CHOICE.EQ.'PREVIOUS FRAME') THEN
            LT1 = MAX (2*CHAN1-CHAN2, 1)
            LT2 = LT1 + CHAN2 - CHAN1
            IF ((CHAN1.NE.LT1) .OR. (CHAN2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               CHAN1 = LT1
               CHAN2 = LT2
               END IF
C                                       1 IF at a time
         ELSE IF (CHOICE.EQ.'SELECT IF') THEN
            IF (EIF-BIF.GT.1) THEN
               WRITE (MSGBUF,1230) BIF, EIF
               CALL INQINT (TTY, MSGBUF, 1, J, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
            ELSE
               J = BIF + 1
               IF (CHNTOT.LE.CHNMAX) J = BIF
               END IF
            IF ((J.LT.BIF) .OR. (J.GT.EIF)) THEN
               CHAN1 = 1
               CHAN2 = CHNTOT
            ELSE
               CHAN1 = 1 + (J-BIF) * CHNMAX
               CHAN2 = CHAN1 + CHNMAX - 1
               END IF
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       1 IF at a time
         ELSE IF (CHOICE.EQ.'NEXT IF') THEN
            J = CHAN2 / CHNMAX + BIF - 1
            J = J + 1
            IF (J.GT.EIF) J = BIF
            CHAN1 = 1 + (J-BIF) * CHNMAX
            CHAN2 = CHAN1 + CHNMAX - 1
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       1 IF at a time
         ELSE IF (CHOICE.EQ.'LAST IF') THEN
            J = CHAN2 / CHNMAX + BIF - 1
            J = J - 1
            IF (J.LT.BIF) J = EIF
            CHAN1 = 1 + (J-BIF) * CHNMAX
            CHAN2 = CHAN1 + CHNMAX - 1
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Show a type
         ELSE IF (CHOICE(:5).EQ.'SHOW ') THEN
            IF (CHOICE.EQ.'SHOW AMPLITUDE') THEN
               LTYPE = 1
               LTYPE2 = 2
               CHOICS(CHSHOW) = 'SHOW PHASE'
            ELSE IF (CHOICE.EQ.'SHOW PHASE') THEN
               LTYPE = 2
               LTYPE2 = 1
               CHOICS(CHSHOW) = 'SHOW AMPLITUDE'
               END IF
            DOPLOT = .TRUE.
            PNDING = .TRUE.
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
               CALL BPPLOT (IERR)
            ELSE IF (PNDING) THEN
               CALL BPPLST (GRSEL(6), IERR)
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
         CALL BPGTBP (IERR)
         IF (IERR.NE.0) GO TO 980
         CALL BPFCAP (UVMAST, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABZAP (FCFILE, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'BPGTBP', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 800 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 800        CONTINUE
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       destroy result
      ELSE IF ((OPCODE.EQ.'ABOR') .OR. (OPCODE.EQ.'KILL')) THEN
         IF ((FCREAT) .AND. (OPCODE.NE.'KILL')) THEN
            PROBLM = FCFILE
            CALL TABZAP (FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'BPGTBP', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 810 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 810        CONTINUE
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       ??
      ELSE
         IERR = 2
         CALL BPSTUB ('EDITSN', OPCODE, OBJECT)
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
 1230 FORMAT ('Enter IF number between',I4,' and',I4)
      END
      SUBROUTINE EDITPC (OPCODE, OBJECT, TBLTYP, IERR)
C-----------------------------------------------------------------------
C   edit a PC table
C   Inputs:
C      OPCODE   C*4    Operation:
C                         'INIT' start a new edit session
C                         'ABOR' close session, delete output table
C                         'KILL' close session, save FC table
C                         'APPL' close session, save output table
C      OBJECT   C*(*)  Open Edit object
C      TBLTYP   C*2    Table type
C   Output:
C      IERR     I      Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C                         < 0 => dies by the users' hands
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OBJECT*(*), TBLTYP*2
      INTEGER   IERR
C
      INTEGER   NCOL1, NCOL2, NCOLS
      PARAMETER (NCOL1 = 21, NCOL2 = 20, NCOLS = NCOL1+NCOL2)
C
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'EDIFCPS.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER   TYPE, DIM(7), SCRTCH(256), I, J, K, TVCOL, TVROWS(2),
     *   TIMLIM, CHS, TVWND(4), MINWIN(2), VERS, IG(8), BUTTON, DUMMY,
     *   TTY(2), ITRIM, IVAL(NPLT), NOZOOM(3), SVZOOM(3), ILAST, LT1,
     *   LT2, TVSTAT(16), GRSTAT(8), JERR, IFLN(2), CHSHOW, CHSHO2,
     *   MSGSAV, AVAL(50), NTITLE, SIDSEP
      REAL      T(8)
      DOUBLE PRECISION DVAL(2), UVFREQ
      LOGICAL   BPAFND, DOPLOT, LOADIT, LEAVE(NCOLS), WANT
      CHARACTER STATUS*4, PROBLM*32, CDUMMY*1, CHOICS(NCOLS)*16,
     *   ISHELP*6, LIST2(NCOL2)*16, MSGBUF*72, DDNAME*18, INKT(4)*8,
     *   INEXT*2, LIST1(NCOL1)*16, REAZON*24, CHOICE*16, TITLE*8,
     *   SHOWS(3)*16, SHOWA(3)*16
      DATA INKT /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
      DATA LIST1 /'FLAG CHANNEL', 'FLAG CHAN RANGE', 'FLAG BELOW',
     *   'FLAG ABOVE', 'FLAG AREA', 'FLAG POINT', 'FLAG QUICKLY',
     *   'ENTER AMPL RNG', 'ENTER PHASE RNG', 'ENTER RESID RNG',
     *   'LIST FLAGS', 'UNDO FLAGS', 'REDO FLAGS', 'REDO DELAYS',
     *   'TV ZOOM', 'OFF ZOOM', 'HOLD TV LOAD', 'REPLOT',' ',
     *   'EXIT', 'ABORT'/
      DATA LIST2 /'SWITCH POLARIZ', 'SWITCH ALL POL',
     *   'SWITCH ALL TIME', 'SWITCH ALL SOURC', 'SWITCH ALL ANT',
     *   'ENTER ANTENNA', 'ENTER OTHER ANT', 'NEXT ANTENNA',
     *   'PLOT ALL CHANNEL', 'SELECT FRAME', 'NEXT FRAME',
     *   'PREVIOUS FRAME', 'SELECT IF', 'NEXT IF', 'LAST IF',
     *   'NEXT TIME', 'LAST TIME', 'SHOW PHASE', 'SHOW RESID',
     *   'SHOW ALSO RESID'/
      DATA SHOWS /'SHOW AMPLITUDE', 'SHOW PHASE', 'SHOW RESID'/
      DATA SHOWA /'SHOW ALSO AMP', 'SHOW ALSO PHASE',
     *   'SHOW ALSO RESID'/
      DATA LEAVE /NCOLS*.TRUE./
C-----------------------------------------------------------------------
      TTY(1) = 5
      TTY(2) = 0
      ALLSOR = .FALSE.
C                                       plot scale ranges
      CALL RFILL (2, 1.0, RSCALE)
C                                       Init the functions
      IF (OPCODE.EQ.'INIT') THEN
         TIMEC = 2
         TIMEL = 0
         FLAGED = .TRUE.
         APOPEN = .FALSE.
         FCREAT = .FALSE.
         FLGMSG = .TRUE.
         FCFILE = ' '
         NEWPLT = .TRUE.
         ILAST = 0
         DTYPE(1) = 'Amplitude'
         DTYPE(2) = 'Phase'
         DTYPE(3) = 'Resid'
         DUNITS(1) = 'Jy ??'
         DUNITS(2) = 'Degrees'
         DUNITS(3) = 'Degrees'
         CALL RFILL (2, 1.0, DPLSCL)
         ALLPOL = .FALSE.
         ALLTIM = .FALSE.
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
         UVFLAG = .TRUE.
C                                       get UV data frequency
         PROBLM = UVMAST
         CALL OOPEN (UVMAST, 'READ', IERR)
         IF (IERR.NE.0) GO TO 980
         CALL UVDGET (UVMAST, 'REFFREQ', TYPE, DIM, UVFREQ, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL OCLOSE (UVMAST, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Make sure that the table is
C                                       sorted correctly
         CALL TBLSRT (TBEDIT, 'TIME', 'ANTENNA_NO', IERR)
         IF (IERR.NE.0) GO TO 999
         PROBLM = TBEDIT
C                                       Open table object: get adverbs
         STATUS = 'READ'
         CALL TABOPN (TBEDIT, STATUS, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Get a source list
         CALL BPSLST (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       FC table for UV flagging
         FCFILE = 'Flag Command table for BP edits'
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
            MSGTXT = '**** WARNING: USING A PRE-EXISTING FC TABLE' //
     *         ' ****'
            CALL MSGWRT (6)
            END IF
         CALL OFCINI (FCFILE, 'WRIT', FLGNUM, FCROW, IERR)
        IF (IERR.NE.0) GO TO 980
         CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *      FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.NE.0) GO TO 980
         FLGNMX = FLGNUM
C                                       get other adverbs
         PROBLM = TBEDIT
         MSGSAV = MSGSUP
         DOCOMP = .TRUE.
         REASON = ' '
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
         DTIME = 1 / 6000.0
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0 * DTIME)
         CALL FILL (256, 0, SCRTCH)
         CALL TABGET (TBEDIT, 'ANTENNAS', TYPE, DIM, SCRTCH, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
C                                       parameters of BP
         CALL OGET (TBEDIT, 'NROW', TYPE, DIM, ROWMAX, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         IFMAX = EIF - BIF + 1
         CALL OGET (TBEDIT, 'KEY.NO_TONES', TYPE, DIM, CHNMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (CHNMAX.LE.0) CHNMAX = 1
         CALL OGET (TBEDIT, 'KEY.NO_POL', TYPE, DIM, POLMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         IF (POLMAX.LE.0) POLMAX = 1
         IF (POLMAX.GT.2) THEN
             WRITE (MSGTXT,1000) POLMAX
             CALL MSGWRT (6)
             POLMAX = 2
             END IF
         ANTMAX = MAXANT
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
         CHAN1 = 1
         CHAN2 = 0
         PIFNOW = -1
         MAXREC = 0
         POLNOW = 1
         LTYPE = 1
         LTYPE2 = 0
         IF (DOCOMP) LTYPE2 = 2
         IFNOW = BIF
C                                       set default ranges to full range
C                                       of phases, -1000:1000 ns & mHz
C                                       self scale in non-expert
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
         CALL BPWINC (MINWIN(1), MINWIN(2), IERR)
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
         CALL TVDOPR (TVNAME, 'HOFF', I, IERR)
         IF (IERR.NE.0) GO TO 980

C                                       Get some data
         WASFLG = -1
         PROBLM = TBEDIT
         CALL PCGTPC (IERR)
         IF (IERR.NE.0) GO TO 980
         IF (POLMAX.LE.1) THEN
            POLNOW = 1
            END IF
C                                       calibrate
         CALL PCALIB
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
     *         (BPAFND(AVAL(I),ANTMLX,ANTEN)) .AND.
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
         CALL BPPLOT (IERR)
         IF (IERR.NE.0) GO TO 980
C                                       set parameters for menu
         TVCOL = 2
         TVROWS(1) = NCOL1
         DO 50 I = 1,NCOL1
            CHOICS(I) = LIST1(I)
 50         CONTINUE
         J = TVROWS(1)
         IF (POLMAX.GT.1) THEN
            J = J + 1
            CHOICS(J) = LIST2(1)
            J = J + 1
            CHOICS(J) = LIST2(2)
            END IF
         IF (EIF.GT.BIF) THEN
            J = J + 1
            CHOICS(J) = LIST2(3)
            END IF
         CHSHO2 = 0
         DO 60 I = 4,NCOL2
            J = J + 1
            CHOICS(J) = LIST2(I)
            IF (CHOICS(J).EQ.'SHOW PHASE') CHSHOW = J
 60         CONTINUE
         IF (.NOT.DOCOMP) J = J - 1
         TVROWS(2) = J - TVROWS(1)
         IF (TIMEM.LE.1) TVROWS(2) = TVROWS(2) - 2
         ISHELP = 'EDIPC'
         TIMLIM = 0
         IG(1) = GRSEL(1)
         IG(2) = GRSEL(2)
C                                       LOOP POINT
C                                       window still big enough?
 100     PROBLM = TVNAME
         CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL BPWINC (MINWIN(1), MINWIN(2), IERR)
         IF (IERR.NE.0) GO TO 980
         IF (NEWPLT) THEN
            CALL TVDOKA (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            GRSTAT(IG(1)) = 1
            CALL TVDRST (TVNAME, TVSTAT, GRSTAT, IERR)
            IF (IERR.NE.0) GO TO 980
            ILAST = I
            END IF
         I = LTVWND(4) - TOPLOT + 5 * CSIZE(2) - 1
         IF (PREXIS) THEN
            CHOICE = 'REDO FLAGS'
         ELSE
            TITLE = ' '
            NTITLE = 0
            SIDSEP = 7
C                                       choices
            J = CHSHOW
            I = MOD (LTYPE, 3)
            CHOICS(J) = SHOWS(I+1)
            I = MOD (I+1, 3)
            J = J + 1
            CHOICS(J) = SHOWS(I+1)
            J = J + 1
            IF (DOCOMP) THEN
               IF (LTYPE.EQ.1) THEN
                  IF (LTYPE2.EQ.2) THEN
                     I = 3
                  ELSE
                     I = 2
                     END IF
               ELSE IF (LTYPE.EQ.2) THEN
                  IF (LTYPE2.EQ.1) THEN
                     I = 3
                  ELSE
                     I = 1
                     END IF
               ELSE
                  IF (LTYPE2.EQ.1) THEN
                     I = 2
                  ELSE
                     I = 1
                     END IF
                  END IF
               CHOICS(J) = SHOWA(I)
               END IF
            I = LTVWND(4) - TOPLOT + 5 * CSIZE(2) - 1
            CALL TVDMEN (TVNAME, -1, TVCOL, TVROWS, IG, I, SIDSEP,
     *         ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHS,
     *         BUTTON, IERR)
            IF (IERR.NE.0) GO TO 980
            CHOICE = CHOICS(CHS)
            END IF
         CALL BPWINC (MINWIN(1), MINWIN(2), IERR)
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
               CALL BPPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL TVDZOM (TVNAME, 'WRIT', SVZOOM, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (CHOICE.EQ.'FLAG CHANNEL') THEN
               CALL BPFLCI (IERR)
            ELSE IF (CHOICE.EQ.'FLAG CHAN RANGE') THEN
               CALL BPFLCR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG BELOW') THEN
               CALL BPFLFB (IERR)
            ELSE IF (CHOICE.EQ.'FLAG ABOVE') THEN
               CALL BPFLFA (IERR)
            ELSE IF (CHOICE.EQ.'FLAG AREA') THEN
               CALL BPFLAR (IERR)
            ELSE IF (CHOICE.EQ.'FLAG POINT') THEN
               CALL BPFLPT (IERR)
            ELSE IF (CHOICE.EQ.'FLAG QUICKLY') THEN
               CALL BPFLQU (IERR)
            ELSE
               CALL BPSTUB ('FLAG', CHOICE, OBJECT)
               END IF
            IF (IERR.NE.0) GO TO 980
            CALL TVDZOM (TVNAME, 'WRIT', NOZOOM, IERR)
C                                       list FC table
         ELSE IF (CHOICE.EQ.'LIST FLAGS') THEN
            CALL BPFCLI (IERR)
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
               CALL BPFCUN (IFLN, IERR)
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
C                                       Redo flags in FC table
         ELSE IF (CHOICE.EQ.'REDO FLAGS') THEN
            CALL BPFCDO (IERR)
            PREXIS = .FALSE.
C                                       redo delay fit
         ELSE IF (CHOICE.EQ.'REDO DELAYS') THEN
            CALL PCALIB
            IF ((LTYPE.EQ.3) .OR. (LTYPE2.EQ.3)) THEN
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
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
            ELSE IF ((BPAFND (I, ANTMLX, ANTEN)) .AND. (MSAMPS(I).GT.0))
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
C                                       next time
         ELSE IF (CHOICE.EQ.'NEXT TIME') THEN
            TIMEC = TIMEC + 1
            IF (TIMEC.GT.TIMEM-2) TIMEC = 2
            DOPLOT = TIMEC.NE.TIMEL
            PNDING = DOPLOT
C                                       next time
         ELSE IF (CHOICE.EQ.'LAST TIME') THEN
            TIMEC = TIMEC - 1
            IF (TIMEC.LT.2) TIMEC = TIMEM - 2
            DOPLOT = TIMEC.NE.TIMEL
            PNDING = DOPLOT
C                                       Select antenna
         ELSE IF (CHOICE.EQ.'NEXT ANTENNA') THEN
            I = PLTAN(1)
 110        I = I + 1
               IF (I.GT.ANTMAX) I = 1
               IF (MSAMPS(I).LE.0) GO TO 110
               IF (.NOT.(BPAFND (I, ANTMLX, ANTEN))) GO TO 110
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
                     IF ((BPAFND (I, ANTMLX, ANTEN)) .AND.
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
                     IF ((BPAFND (IVAL(I), ANTMLX, ANTEN)) .AND.
     *                  (MSAMPS(IVAL(I)).GT.0)) THEN
                        NUMPLT = NUMPLT + 1
                        PLTAN(NUMPLT) = IVAL(I)
                        END IF
                     END IF
 210              CONTINUE
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               IF (K.NE.NUMPLT) THEN
                  CALL BPWINC (MINWIN(1), MINWIN(2), IERR)
                  IF (IERR.NE.0) GO TO 980
                  NEWPLT = .TRUE.
                  ILAST = 0
                  END IF
            ELSE
               MSGTXT = 'NUMBER ERROR: COMMAND IGNORED'
               CALL MSGWRT (6)
               IERR = 0
               END IF
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
            IF (CHOICE.EQ.'ENTER RESID RNG') I = 3
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
               CALL BPSTUB ('ENTER', CHOICE, OBJECT)
               END IF
C                                       Switch polarization
         ELSE IF (CHOICE.EQ.'SWITCH POLARIZ') THEN
            POLNOW = 3 - POLNOW
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Switch all ant
         ELSE IF (CHOICE.EQ.'SWITCH ALL ANT') THEN
            IF (ALLANT.GT.0) THEN
               ALLANT = 0
            ELSE
               ALLANT = PLTAN(1)
               END IF
            CALL BPPLST (GRSEL(6), IERR)
C                                       Switch all pol
         ELSE IF (CHOICE.EQ.'SWITCH ALL POL') THEN
            IF (POLMAX.GT.1) THEN
               ALLPOL = .NOT.ALLPOL
               CALL BPPLST (GRSEL(6), IERR)
               END IF
C                                       Switch all pol
         ELSE IF (CHOICE.EQ.'SWITCH ALL TIME') THEN
            ALLTIM = .NOT.ALLTIM
            CALL BPPLST (GRSEL(6), IERR)
C                                       Switch all source
         ELSE IF (CHOICE.EQ.'SWITCH ALL SOURC') THEN
            ALLSOR = .NOT.ALLSOR
            CALL BPPLST (GRSEL(6), IERR)
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
C                                       full plot
         ELSE IF (CHOICE.EQ.'PLOT ALL CHANNEL') THEN
            IF ((CHAN1.GT.1) .OR. (CHAN2.LT.CHNTOT)) THEN
               CHAN1 = 1
               CHAN2 = CHNTOT
               DOPLOT = .TRUE.
               PNDING = .TRUE.
               END IF
C                                       Select frame
         ELSE IF (CHOICE.EQ.'SELECT FRAME') THEN
            IF ((CHAN1.GT.1) .OR. (CHAN2.LT.CHNTOT) .OR. PNDING) THEN
               CHAN1 = 1
               CHAN2 = CHNTOT
               CALL BPPLOT (IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL BPFRAM (IERR)
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Next frame
         ELSE IF (CHOICE.EQ.'NEXT FRAME') THEN
            LT2 = MIN (2*CHAN2-CHAN1, CHNTOT)
            LT1 = LT2 - CHAN2 + CHAN1
            IF ((CHAN1.NE.LT1) .OR. (CHAN2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               CHAN1 = LT1
               CHAN2 = LT2
               END IF
C                                       Previous frame
         ELSE IF (CHOICE.EQ.'PREVIOUS FRAME') THEN
            LT1 = MAX (2*CHAN1-CHAN2, 1)
            LT2 = LT1 + CHAN2 - CHAN1
            IF ((CHAN1.NE.LT1) .OR. (CHAN2.NE.LT2)) DOPLOT = .TRUE.
            IF (DOPLOT) THEN
               PNDING = .TRUE.
               CHAN1 = LT1
               CHAN2 = LT2
               END IF
C                                       1 IF at a time
         ELSE IF (CHOICE.EQ.'SELECT IF') THEN
            IF (EIF-BIF.GT.1) THEN
               WRITE (MSGBUF,1230) BIF, EIF
               CALL INQINT (TTY, MSGBUF, 1, J, IERR)
               PROBLM = 'The terminal'
               IF (IERR.GT.0) GO TO 980
            ELSE
               J = BIF + 1
               IF (CHNTOT.LE.CHNMAX) J = BIF
               END IF
            IF ((J.LT.BIF) .OR. (J.GT.EIF)) THEN
               CHAN1 = 1
               CHAN2 = CHNTOT
            ELSE
               CHAN1 = 1 + (J-BIF) * CHNMAX
               CHAN2 = CHAN1 + CHNMAX - 1
               END IF
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       1 IF at a time
         ELSE IF (CHOICE.EQ.'NEXT IF') THEN
            J = CHAN2 / CHNMAX + BIF - 1
            J = J + 1
            IF (J.GT.EIF) J = BIF
            CHAN1 = 1 + (J-BIF) * CHNMAX
            CHAN2 = CHAN1 + CHNMAX - 1
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       1 IF at a time
         ELSE IF (CHOICE.EQ.'LAST IF') THEN
            J = CHAN2 / CHNMAX + BIF - 1
            J = J - 1
            IF (J.LT.BIF) J = EIF
            CHAN1 = 1 + (J-BIF) * CHNMAX
            CHAN2 = CHAN1 + CHNMAX - 1
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       comparison type
         ELSE IF (CHOICE(:10).EQ.'SHOW ALSO ') THEN
            IF (CHOICE(11:).EQ.'AMP') THEN
               LTYPE2 = 1
            ELSE IF (CHOICE(11:).EQ.'PHASE') THEN
               LTYPE2 = 2
            ELSE IF (CHOICE(11:).EQ.'RESID') THEN
               LTYPE2 = 3
               END IF
            DOPLOT = .TRUE.
            PNDING = .TRUE.
C                                       Show a type
         ELSE IF (CHOICE(:5).EQ.'SHOW ') THEN
            IF (CHOICE.EQ.'SHOW AMPLITUDE') THEN
               LTYPE = 1
               IF (LTYPE2.EQ.1) LTYPE2 = 2
            ELSE IF (CHOICE.EQ.'SHOW PHASE') THEN
               LTYPE = 2
               IF (LTYPE2.EQ.2) LTYPE2 = 1
            ELSE IF (CHOICE.EQ.'SHOW RESID') THEN
               LTYPE = 3
               IF (LTYPE2.EQ.3) LTYPE2 = 1
               END IF
            DOPLOT = .TRUE.
            PNDING = .TRUE.
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
               CALL BPPLOT (IERR)
            ELSE IF (PNDING) THEN
               CALL BPPLST (GRSEL(6), IERR)
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
         CALL PCGTPC (IERR)
         IF (IERR.NE.0) GO TO 980
         CALL PCFCAP (TBEDIT, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TABZAP (FCFILE, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'PCGTPC', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 800 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 800        CONTINUE
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       destroy result
      ELSE IF ((OPCODE.EQ.'ABOR') .OR. (OPCODE.EQ.'KILL')) THEN
         IF ((FCREAT) .AND. (OPCODE.NE.'KILL')) THEN
            PROBLM = FCFILE
            CALL TABZAP (FCFILE, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         MSGTXT = 'Deleting output PC table because of ' // OPCODE
         CALL MSGWRT (6)
         CALL TABZAP (TBEDIT, IERR)
C                                       Give the AP memory
         IF (APOPEN) CALL ZMEMRY ('FREE', 'PCGTPC', EDSIZE, EDCORE,
     *       EDIPTR, JERR)
         CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
         DO 810 I = 1,NGRPH
            CALL TVDOPR (TVNAME, 'GROFF', I, JERR)
 810        CONTINUE
         CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
C                                       ??
      ELSE
         IERR = 2
         CALL BPSTUB ('EDITSN', OPCODE, OBJECT)
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
 1230 FORMAT ('Enter IF number between',I4,' and',I4)
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   Private functions:
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      LOGICAL FUNCTION BPAFND (IA, NANT, ANTENS)
C-----------------------------------------------------------------------
C   returns true if antenna IA in list ANTENS(NANT)
C   Inputs:
C      IA       I      test number
C      NANT     I      length of list
C      ANTENS   I(*)   list of numbers
C   Output:
C      BPAFND   L      number is in list
C-----------------------------------------------------------------------
      INTEGER   IA, NANT, ANTENS(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      BPAFND = .FALSE.
      IF (NANT.GT.0) THEN
         DO 10 I = 1,NANT
            IF (IA.EQ.ANTENS(I)) BPAFND = .TRUE.
 10         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE BPANTS (ANTS, ANT1, ANT2)
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
      SUBROUTINE BPDBOX (NAME, TYPE, MODE, CHAN, CORN, IERR)
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
      SUBROUTINE BPFCAP (UVDATA, IERR)
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
            CALL BPANTS (ABS(FLGANT(2)), FGANTS(1), FGANTS(2))
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
 980  MSGTXT = 'BPFCAP: PROBLEM WITH ' // PROBLM
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
 1020 FORMAT ('BPFCAP: copied',I5,' rows from FG table version',I4)
 1070 FORMAT ('BPFCAP: added ',I5,' rows to   FG table version',I4)
      END
      SUBROUTINE PCFCAP (TBDATA, IERR)
C-----------------------------------------------------------------------
C   Applies an FC table to a PC table
C   Inputs:
C      TBDATA   C*(*)   Table to be edited
C   Output:
C      IERR     I       Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C   UV Editor common is used: FCFILE
C   The FCFILE is left to later disposal.
C-----------------------------------------------------------------------
      CHARACTER TBDATA*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PPCV.INC'
      INTEGER   NKEY1, NVAL
      PARAMETER (NKEY1 = 4)
      PARAMETER (NVAL = MAXTON*MAXIF)
C
      CHARACTER CDUMMY*1, PROBLM*32
      INTEGER   DIM(7), TYPE, FCOPEN, JERR, LASTR, IROW, IFQ, SUBA,
     *   SOURC, IAN, FI, FI1, FI2, FL, FL1, FL2, IC, IP, ICL, JFLAG,
     *   NFLAG, JROW
      REAL      VALUES(NVAL,4)
      DOUBLE PRECISION TIME
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIFCPS.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      FCOPEN = 0
C                                       open FC FILE
      PROBLM = FCFILE
      CALL OFCINI (FCFILE, 'READ', FLGNUM, LASTR, IERR)
      IF (IERR.NE.0) GO TO 980
      FCOPEN = 1
      IF ((LASTR.LE.0) .OR. (FLGNUM.LE.0)) THEN
         MSGTXT = 'Deleting output PC table since no flags'
         CALL MSGWRT (4)
         CALL TABZAP (TBDATA, IERR)
         GO TO 995
         END IF
C                                       read through PC table
      NFLAG = 0
      DO 100 IROW = 1,ROWMAX
C                                       parameters
         ICL = 4
         CALL TABDGT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         IF (IERR.LT.0) GO TO 100
         ICL = 5
         CALL TABDGT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM, SUBA,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         ICL = 6
         CALL TABDGT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM, SOURC,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         ICL = 1
         CALL TABDGT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM, IAN,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         ICL = 2
         CALL TABDGT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM, TIME,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       data
         ICL = 7
         CALL TABDGT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM, VALUES(1,1),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         ICL = 8
         CALL TABDGT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM, VALUES(1,2),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         IF (POLMAX.GT.1) THEN
            ICL = 10
            CALL TABDGT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM,
     *         VALUES(1,3), CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
            ICL = 11
            CALL TABDGT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM,
     *         VALUES(1,4), CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       does one++ FC table row apply
         FCROW = 1
         JFLAG = 0
         DO 50 JROW = 1,LASTR
            CALL OTABFC (FCFILE, 'READ', FCROW, FLGTIM, FLGANT, FLGSOR,
     *         FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *         FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
            IF (IERR.GT.0) GO TO 980
            IF ((TIME.LT.FLGTIM(1)) .OR. (TIME.GT.FLGTIM(2))) GO TO 50
            IF ((FLGANT(2).NE.0) .AND. (FLGANT(1).GT.0) .AND.
     *         (FLGANT(1).NE.IAN)) GO TO 50
            IF ((FLGSOR.GT.0) .AND. (FLGSOR.NE.SOURC)) GO TO 50
            IF ((FLGSUB.GT.0) .AND. (FLGSUB.NE.SUBA)) GO TO 50
            IF ((FLGFQ.GT.0) .AND. (FLGFQ.NE.IFQ)) GO TO 50
            IF (FLGCHN(1).LE.0) THEN
               FL1 = 1
               FL2 = CHNMAX
            ELSE
               FL1 = FLGCHN(1)
               FL2 = FLGCHN(2)
               END IF
            IF (FLGIF(1).LE.0) THEN
               FI1 = BIF
               FI2 = EIF
            ELSE
               FI1 = FLGIF(1)
               FI2 = FLGIF(2)
               END IF
            DO 40 POLNOW = 1,POLMAX
               IF (FLGSTK(POLNOW:POLNOW).EQ.'1') THEN
                  IP = (POLNOW-1) * 2 + 1
                  DO 30 FI = FI1,FI2
                     IC = CHNMAX * (FI-1) + FL1 - 1
                     DO 20 FL = FL1,FL2
                        IC = IC + 1
                        IF (VALUES(IC,IP).NE.FBLANK) THEN
                           VALUES(IC,IP) = FBLANK
                           JFLAG = 1
                           NFLAG = NFLAG + 1
                           END IF
                        VALUES(IC,IP+1) = FBLANK
 20                     CONTINUE
 30                  CONTINUE
                  END IF
 40            CONTINUE
 50         CONTINUE
         IF (JFLAG.GT.0) THEN
            ICL = 7
            CALL TABDPT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM,
     *         VALUES(1,1), CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
            ICL = 8
            CALL TABDPT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM,
     *         VALUES(1,2), CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
            IF (POLMAX.GT.1) THEN
               ICL = 10
               CALL TABDPT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM,
     *            VALUES(1,3), CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 995
               ICL = 11
               CALL TABDPT (TBDATA, IROW, COLNUM(ICL), TYPE, DIM,
     *            VALUES(1,4), CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 995
               END IF
            END IF
 100     CONTINUE
      WRITE (MSGTXT,1100) NFLAG
      CALL MSGWRT (5)
      GO TO 995
C
 980  MSGTXT = 'PCFCAP: PROBLEM WITH ' // PROBLM
      CALL MSGWRT (7)
C
 995  IF (FCOPEN.GT.0) CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM,
     *   FLGANT, FLGSOR, FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM,
     *   FLGOP, FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('PCFCAP: flagged',I10,' PC samples')
      END
      SUBROUTINE BPFCDO (IERR)
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
      INTEGER   IROW, LASTR, ITYPE, I, INLANT, NFL, NFLT, POLOLD, IFOLD,
     *   INTIMC, FOP, FLGITS(2)
      CHARACTER FLGOPS(6)*8
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIFCPS.INC'
      LOGICAL   INUVFL, INLPOL, DONEIF(MAXIF,2), INTIME
      DATA FLGOPS / 'CHANNEL', 'CHNRANGE', 'BELOW', 'ABOVE', 'AREA',
     *   'POINT'/
C-----------------------------------------------------------------------
C                                       open table for read
      INUVFL = UVFLAG
      INLANT = ALLANT
      INLPOL = ALLPOL
      INTIME = ALLTIM
      INTIMC = TIMEC
      NFLT = 0
      POLOLD = POLNOW
      IFOLD = IFNOW
      PLTPOL = POLNOW
      FLGMSG = .FALSE.
      UVFLAG = .FALSE.
      CALL OFCINI (FCFILE, 'READ', FLGNUM, LASTR, IERR)
      IF (IERR.NE.0) GO TO 980
      FLGNMX = FLGNUM
C                                       loop reading
      FCROW = 1
      DO 20 IROW = 1,LASTR
         CALL OTABFC (FCFILE, 'READ', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *      FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.GT.0) GO TO 980
         IF ((IERR.EQ.0) .AND. ((DIMIF.GT.1) .OR. (IFNOW.EQ.0) .OR.
     *      (IFNOW.EQ.FLGIF(1)) .OR. (FLGIF(1).EQ.0))) THEN
            ITYPE = 1
            DO 10 I = 1,2
               IF (LDTYPE.EQ.DTYPE(I)(:8)) ITYPE = I
 10            CONTINUE
            ALLANT = FLGANT(2)
            I = EIF - BIF + 1
            CALL LFILL (I, .TRUE., DONEIF(BIF,1))
            CALL LFILL (I, .TRUE., DONEIF(BIF,2))
            DONEIF(FLGIF(1),1) = .FALSE.
            DONEIF(FLGIF(1),2) = .FALSE.
            ALLPOL = FLGSTK(1:2).EQ.'11'
            CALL BPTIMI (FLGTIM, FLGITS)
            IF (FLGITS(1).EQ.FLGITS(2)) THEN
               ALLTIM = .FALSE.
               TIMEC = FLGITS(1)
            ELSE
               ALLTIM = .TRUE.
               END IF
            FOP = 0
            DO 11 I = 1,6
               IF (FLGOP.EQ.FLGOPS(I)) FOP = I
 11            CONTINUE
            DO 15 IFNOW = BIF,EIF
               DO 14 POLNOW = 1,2
                  IF ((FLGSTK(POLNOW:POLNOW).EQ.'1') .AND.
     *               (.NOT.DONEIF(IFNOW,POLNOW)) .AND.
     *               ((IFNOW.GE.FLGIF(1)) .AND. (IFNOW.LE.FLGIF(2))))
     *               THEN
                     I = FLGIF(2) - FLGIF(1) + 1
                     CALL LFILL (I, .TRUE., DONEIF(FLGIF(1),POLNOW))
                     IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                  DONEIF(FLGIF(1),3-POLNOW))
                     CALL BPFLAG ('FLAG', FOP, FLGANT(1), ITYPE, FLGCHN,
     *                  DFLUXS, NFL, IERR)
                     IF (IERR.GT.0) GO TO 980
                     NFLT = NFLT + NFL
                  END IF
 14               CONTINUE
 15            CONTINUE
            POLNOW = POLOLD
            IFNOW = IFOLD
            END IF
 20      CONTINUE
      FLGMSG = .TRUE.
C                                       close
      CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *   FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT,
     *   LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
      IF (IERR.GT.0) GO TO 980
      IF (NFLT.GT.0) THEN
         WRITE (MSGTXT,1020) NFLT
         CALL MSGWRT (3)
         END IF
      GO TO 990
C
 980  MSGTXT = 'BPFCDO: ERROR WITH ' // FCFILE
      CALL MSGWRT (6)
C
 990  UVFLAG = INUVFL
      ALLANT = INLANT
      ALLPOL = INLPOL
      ALLTIM = INTIME
      TIMEC = INTIMC
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('BPFCDO: flagged',I7,' points not previously flagged')
      END
      SUBROUTINE BPFCLI (IERR)
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
      CALL OFCINI (FCFILE, 'READ', FLGNUM, LASTR, IERR)
      IF (IERR.NE.0) GO TO 980
      FLGNMX = FLGNUM
      WRITE (MSGTXT,1000) LASTR, FLGNUM
      CALL MSGWRT (3)
      WRITE (MSGTXT,1005)
      IF (LASTR.GT.0) CALL MSGWRT (3)
C                                       loop reading
      FCROW = 1
      LLGNUM = 0
      DO 20 IROW = 1,LASTR
         CALL  OTABFC (FCFILE, 'READ', FCROW, FLGTIM, FLGANT, FLGSOR,
     *      FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP,
     *      FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
         IF (IERR.GT.0) GO TO 980
         IF ((IERR.EQ.0) .AND. (FLGNUM.NE.LLGNUM)) THEN
            LLGNUM = FLGNUM
            CALL TODHMS (DTIMES(1), ITT(1))
            CALL TODHMS (DTIMES(2), ITT(5))
            CALL BPANTS (ABS(FLGANT(2)), ANT1, ANT2)
            IF (ANT1.GT.0) THEN
               WRITE (MSGTXT,1010) FLGNUM, IROW, FLGOP, ANT1,
     *            FLGIF, ITT, FLGSTK
            ELSE
               WRITE (MSGTXT,1011) FLGNUM, IROW, FLGOP, FLGIF,
     *            ITT, FLGSTK
               END IF
            CALL MSGWRT (3)
C                                       fluxes used
            IF (FLGOP(:2).NE.'CH') THEN
               WRITE (MSGTXT,1020) DFLUXS, LDTYPE
               CALL MSGWRT (3)
               END IF
C                                       reason
            IF ((FLGREA.NE.TSKNAM(:5)//':date time') .AND.
     *         (FLGREA.NE.' ')) THEN
               MSGTXT = ' '
               I = 29
               IF (DDTYPE.NE.'UV') I = 28
               MSGTXT(I:) = 'Reason = ''' // FLGREA(:ITRIM(FLGREA))
     *            // ''''
               CALL MSGWRT (3)
               END IF
            END IF
 20      CONTINUE
C                                       close
      CALL OTABFC (FCFILE, 'CLOS', FCROW, FLGTIM, FLGANT, FLGSOR,
     *   FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT,
     *   LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
      IF (IERR.GT.0) GO TO 980
      GO TO 999
C
 980  MSGTXT = 'BPFCLI: ERROR WITH ' // FCFILE
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FC file with',I8,' rows and',I5,' flag commands:')
 1005 FORMAT (' FC#  Row Operation Ant   IFs',10X,'Time range',8X,
     *   'Stokes')
 1010 FORMAT (I4,I5,2X,A8,2I4,I3,2(I4.1,'/',2(I2.2,':'),I2.2),2X,A)
 1011 FORMAT (I4,I5,2X,A8,'   *',I4,I3,2(I4.1,'/',2(I2.2,':'),I2.2),
     *   2X,A)
 1020 FORMAT (27X,2(1PE13.4),2X,A)
      END
      SUBROUTINE BPFCUN (ENTRY, IERR)
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
     *   POLOLD, IFOLD, INTIMC, FOP
      CHARACTER FCTEMP*32, PROBLM*32, CDUMMY*1, FLGOPS(6)*8
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIFCPS.INC'
      LOGICAL   EXIST, INPOL, DONEIF(MAXIF,2), DOCOPY, INTIME
      INCLUDE 'INCS:PAOOF.INC'
      DATA FLGOPS / 'CHANNEL', 'CHNRANGE', 'BELOW', 'ABOVE', 'AREA',
     *   'POINT'/
C-----------------------------------------------------------------------
C                                       open table for read
      INLANT = ALLANT
      INPOL  = ALLPOL
      INTIME = ALLTIM
      INTIMC = TIMEC
      NFLT = 0
      NSKIP = 0
      POLOLD = POLNOW
      IFOLD = IFNOW
      FLGMSG = .FALSE.
      PROBLM = FCFILE
      CALL OFCINI (FCFILE, 'READ', FLGNUM, LASTR, IERR)
      IF (IERR.NE.0) GO TO 980
      IF ((ENTRY(2).LT.1) .OR. (ENTRY(1).GT.FLGNUM) .OR.
     *   (ENTRY(2).LT.ENTRY(1))) THEN
         NSKIP = -1
         WRITE (MSGTXT,1000) ENTRY, FLGNUM
         IF (FLGNUM.EQ.0) MSGTXT = 'BPFCUN: No flags left to undo'
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
            CALL OTABFC (FCFILE, 'READ', FCROW, FLGTIM, FLGANT, FLGSOR,
     *         FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM,FLGOP,
     *         FLGIT, LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
            IF (IERR.GT.0) GO TO 980
C                                       check for unavailable data
            DOCOPY = (FLGNUM.LT.ENTRY(1)) .OR. (FLGNUM.GT.ENTRY(2))
C                                       copy to output
            IF (DOCOPY) THEN
               IF (LLGNUM.NE.FLGNUM) OFLGNM = OFLGNM + 1
               LLGNUM = FLGNUM
               PROBLM = FCTEMP
               CALL  OTABFC (FCTEMP, 'WRIT', OFCROW, FLGTIM, FLGANT,
     *            FLGSOR, FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ,
     *            OFLGNM, FLGOP, FLGIT, LDTYPE, DTIMES, DFLUXS,
     *            FLGREA, IERR)
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
               DO 10 I = 1,2
                  IF (LDTYPE.EQ.DTYPE(I)(:8)) ITYPE = I
 10               CONTINUE
               ALLANT = FLGANT(2)
               I = EIF - BIF + 1
               CALL LFILL (I, .TRUE., DONEIF(BIF,1))
               CALL LFILL (I, .TRUE., DONEIF(BIF,2))
               DONEIF(FLGIF(1),1) = .FALSE.
               DONEIF(FLGIF(1),2) = .FALSE.
               ALLPOL = FLGSTK.EQ.'1111'
               CALL BPTIMI (FLGTIM, FLGITS)
               PLTPOL = POLOLD
               IF (FLGITS(1).EQ.FLGITS(2)) THEN
                  ALLTIM = .FALSE.
                  TIMEC = FLGITS(1)
               ELSE
                  ALLTIM = .TRUE.
                  END IF
               FOP = 0
               DO 11 I = 1,6
                  IF (FLGOP.EQ.FLGOPS(I)) FOP = I
 11               CONTINUE
               DO 15 IFNOW = BIF,EIF
                  DO 14 POLNOW = 1,2
                     IF ((FLGSTK(POLNOW:POLNOW).EQ.'1') .AND.
     *                  (.NOT.DONEIF(IFNOW,POLNOW)) .AND.
     *                  ((IFNOW.GE.FLGIF(1)) .AND. (IFNOW.LE.FLGIF(2))))
     *                  THEN
                        I = FLGIF(2) - FLGIF(1) + 1
                        CALL LFILL (I, .TRUE., DONEIF(FLGIF(1),POLNOW))
                        IF (ALLPOL) CALL LFILL (I, .TRUE.,
     *                     DONEIF(FLGIF(1),3-POLNOW))
                        CALL BPFLAG ('UNFL', FOP, FLGANT(1), ITYPE,
     *                     FLGCHN, DFLUXS, NFL, IERR)
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
      CALL OTABFC (FCTEMP, 'CLOS', OFCROW, FLGTIM, FLGANT, FLGSOR,
     *   FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, OFLGNM, FLGOP, FLGIT,
     *   LDTYPE, DTIMES, DFLUXS, FLGREA, IERR)
      IF (IERR.GT.0) GO TO 980
      CALL TABZAP (FCTEMP, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 990
C
 980  MSGTXT = 'BPFCUN: ERROR WITH ' // PROBLM
      CALL MSGWRT (6)
C
 990  ALLANT = INLANT
      ALLPOL = INPOL
      ALLTIM = INTIME
      TIMEC = INTIMC
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPFCUN: FLAG COMMANDS',2I5,' OUTSIDE RANGE 1 -',I5)
 1020 FORMAT ('BPFCUN: restored',I6,' points in T range',2I7)
 1021 FORMAT ('BPFCUN: removed',I6,' records from flag command table')
      END
      SUBROUTINE BPFGAP (UVDATA, IERR)
C-----------------------------------------------------------------------
C   Applies a uv data set FG table to table data now in core
C   Inputs:
C      UVDATA   C*(*)   UV master data set being flagged
C   Output:
C      IERR     I       Error code: 0 => all is still well
C                         > 0 => dies of unnatural causes
C   UV Editor common is used
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      INTEGER   IERR
C
      INTEGER   NKEY1
      PARAMETER (NKEY1 = 4)
      CHARACTER FGFILE*32, CDUMMY*1, OUTK1(NKEY1)*8, FGVER*32, INEXT*2,
     *   PROBLM*32, REAZON*24, FLGREA*24
      LONGINT   LPTR, JPTR, KPTR
      INTEGER   DIM(7), TYPE, VERS, FGOPEN, JERR, LASTR, DUMMY, FGROW,
     *   FGANTS(2), NADD, LF1, LF2, KF1, KF2, LR, LT, IPOL, LF, FLGSOR,
     *   FLGCHN(2), FLGIF(2), FLGSUB, FLGFQ, FGREC, LIF
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
      LF1 = BIF
      LF2 = EIF
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
         IF (FLGCHN(1).LE.0) FLGCHN(1) = 1
         IF (FLGCHN(2).LE.0) FLGCHN(2) = CHNMAX
C                                       does it apply?
         IF ((IERR.EQ.0) .AND. ((FGANTS(1).EQ.0) .OR. (FGANTS(2).EQ.0))
     *      .AND. ((FLGSUB.EQ.SUBARR) .OR. (FLGSUB.LE.0)) .AND.
     *      (FLGIF(1).LE.LF2) .AND. (FLGIF(2).GE.LF1) .AND.
     *      ((FLGFQ.LE.0) .OR. (FRQSEL.LE.0) .OR. (FRQSEL.EQ.FLGFQ))
     *      .AND. (FLGTIM(1).LT.TEND) .AND. (FLGTIM(2).GT.TSTART) .AND.
     *      ((PFLAGS(1)) .OR. ((PFLAGS(2)) .AND. (POLMAX.GT.1)))) THEN
            NOANT = (FGANTS(1).EQ.0) .AND. (FGANTS(2).EQ.0)
            KF1 = MAX (LF1, FLGIF(1))
            KF2 = MIN (LF2, FLGIF(2))
            IF (FLGCHN(1).LE.0) FLGCHN(1) = 1
            IF (FLGCHN(2).LT.FLGCHN(1)) FLGCHN(2) = CHNMAX
            DO 50 LR = 1,MAXREC
               LPTR = DPTR + (LR - 1) * DIMREC
               LT = EDCORI(LPTR+2)
               IF (FLGTIM(2).LT.TIMES(EDIPTR+LT)) GO TO 100
C                                       antenna & time match
               IF (((NOANT) .OR. (EDCORI(LPTR+1).EQ.FGANTS(1)) .OR.
     *            (EDCORI(LPTR+1).EQ.FGANTS(2))) .AND.
     *            (FLGTIM(1).LE.TIMES(EDIPTR+LT)) .AND. ((FLGSOR.LE.0)
     *            .OR. (EDCORI(SUPTR+LT).LE.0) .OR.
     *            (EDCORI(SUPTR+LT).EQ.FLGSOR))) THEN
                  DO 40 IPOL = 1,POLMAX
                     IF (PFLAGS(IPOL)) THEN
                        JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
                        DO 30 LIF = KF1,KF2
                           KPTR = JPTR + (LIF-BIF) * DIMDAT +
     *                        (FLGCHN(1)-1) * 3
                           DO 20 LF = FLGCHN(1),FLGCHN(2)
                              IF (EDCORE(KPTR+1).GT.0.0) THEN
                                 NADD = NADD + 1
                                 EDCORE(KPTR+1) = 0.0
                                 END IF
                              KPTR = KPTR + NUMVAL
 20                           CONTINUE
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
 980  MSGTXT = 'BPFGAP: PROBLEM WITH ' // PROBLM
      CALL MSGWRT (7)
C
 990  IF (FGOPEN.EQ.2) CALL OTABFG (FGFILE, 'CLOS', FGROW, FLGSOR,
     *   FLGSUB, FLGFQ, FGANTS, FLGTIM, FLGIF, FLGCHN, PFLAGS, REAZON,
     *   JERR)
      IF (FGOPEN.GT.0) CALL TABDES (FGFILE, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('BPFGAP: flagged',I9,' samples using uvdata flag table',
     *   I2)
      END
      SUBROUTINE BPFIND (MODE, WTYPE, IAN, TYPE, TVXY, X, JTCH, IR,
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
C      JTCH   I      Channel number of nearest in total channels
C      IR     I      Record number of nearest
C      IERR   I      Error code: > 0 serious, < 0 none found
C-----------------------------------------------------------------------
      INTEGER   MODE, WTYPE, IAN, TYPE, JTCH, IR, IERR
      REAL      TVXY(2), X(3)
C
      LONGINT   LPTR, JPTR, IPTR
      INTEGER   J, LR1, LR2, LR, IT2, IR2, IT1, IR1, ICHI, ICLO, NL,
     *   ITL(5000), IRL(5000), NLU, I, LMODE, IPL(5000), IFL(5000)
      REAL      V, V2, V1, W2, W1, XS, VTL(5000), WTL(5000), YS, VM, TT
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      COMMON /EDISCR/ ITL, IRL, IPL, IFL, VTL, WTL
C-----------------------------------------------------------------------
      IERR = 0
      XS = REAL (XYPLOT(3,1) - XYPLOT(1,1) - 2*LEDG) / MAX (1.0,
     *   REAL (CHAN2 - CHAN1))
      X(1) = (TVXY(1) - XYPLOT(1,1) - LEDG) / XS + CHAN1
      IF (XS.GE.1.0) THEN
         LMODE = MAX (3, MODE)
         IF (MODE.EQ.0) LMODE = 0
         ICHI = X(1) + 0.999
         ICLO = X(1)
         LMODE = 3
      ELSE
         LMODE = MODE
         IF (MODE.GT.3) LMODE = 6 - MODE
         ICHI = X(1) + 0.499 / XS
         ICLO = X(1) - 0.500 / XS
         END IF
C                                       find a sample up
      NL = 0
      IR2 = 0
      IT2 = 0
      LR1 = EDCORI(PPTR+TIMEC)
      LR2 = EDCORI(PPTR+TIMEC+1) - 1
      IF (LR2.GE.LR1) THEN
         DO 20 LR = LR1,LR2
            LPTR = DPTR + (LR - 1) * DIMREC
            IF (IAN.EQ.EDCORI(LPTR+1)) THEN
               IPTR = DIMPRM + (POLNOW-1) * DIMIF * DIMDAT + LPTR
               J = MAX (1, ICLO - 1)
               JPTR = IPTR + (J-1)*NUMVAL
 10            IF ((EDCORE(JPTR+1).NE.0.0) .AND.
     *            (WTYPE*EDCORE(JPTR+1).GE.0.0)) THEN
                  V = EDCORE(JPTR+TYPE+1)
                  IF (V.NE.FBLANK) THEN
                     IF ((LMODE.GT.0) .AND. (LMODE.LT.3) .AND.
     *                  (J.GT.ICHI) .AND. (IT2.NE.0)) GO TO 40
                     V2 = V
                     IT2 = J
                     IR2 = LR
                     W2 = EDCORE(JPTR+1)
                     IF ((IT2.GE.X(1)) .AND. (LMODE.GE.3)) GO TO 40
                     IF (LMODE.EQ.0) THEN
                        IF ((NL.GT.0) .AND. (J.GT.ICHI+4)) GO TO 40
                        NL = NL + 1
                        VTL(NL) = V2
                        WTL(NL) = W2
                        IRL(NL) = IR2
                        ITL(NL) = IT2
                        IF ((NL.GT.0) .AND. (J.GT.ICHI+4)) GO TO 40
                        IF (NL.EQ.2500) GO TO 40
                        END IF
                     END IF
                  END IF
               J = J + 1
               JPTR = JPTR + NUMVAL
               IF (J.LE.CHNTOT) GO TO 10
               END IF
 20         CONTINUE
         END IF
C                                       find a sample down
 40   J = ICHI + 1
      J = MIN (J, CHNTOT)
      NLU = NL
      IR1 = 0
      IT1 = 0
      LR1 = EDCORI(PPTR+TIMEC)
      LR2 = EDCORI(PPTR+TIMEC+1) - 1
      IF (LR2.GE.LR1) THEN
         DO 60 LR = LR1,LR2
            LPTR = DPTR + (LR - 1) * DIMREC
            IF (IAN.EQ.EDCORI(LPTR+1)) THEN
               IPTR = DIMPRM + (POLNOW-1) * DIMIF * DIMDAT + LPTR
               JPTR = IPTR + (J - 1) * NUMVAL
 50            IF ((EDCORE(JPTR+1).NE.0.0) .AND.
     *            (WTYPE*EDCORE(JPTR+1).GE.0.0)) THEN
                  V = EDCORE(JPTR+TYPE+1)
                  IF (V.NE.FBLANK) THEN
                     IF ((LMODE.GT.0) .AND. (LMODE.LT.3) .AND.
     *                  (J.LT.ICLO) .AND. (IT1.NE.0)) GO TO 90
                     V1 = V
                     IT1 = J
                     IR1 = LR
                     W1 = EDCORE(JPTR+1)
                     IF ((IT1.LE.X(1)) .AND. (LMODE.GE.3)) GO TO 90
                     IF (LMODE.EQ.0) THEN
                        IF ((NL.GT.NLU) .AND. (J.LT.ICLO-4)) GO TO 90
                        NL = NL + 1
                        VTL(NL) = V1
                        WTL(NL) = W1
                        IRL(NL) = IR1
                        ITL(NL) = IT1
                        IF ((NL.GT.NLU) .AND. (J.LT.ICLO-4)) GO TO 90
                        IF (NL.EQ.5000) GO TO 90
                        END IF
                     END IF
                  END IF
               J = J - 1
               JPTR = JPTR - NUMVAL
               IF (J.GE.1) GO TO 50
               END IF
 60         CONTINUE
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
            TT = (ITL(I) - CHAN1) * XS + XYPLOT(1,1) + LEDG
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
            J = 1
            END IF
         END IF
      IF (J.EQ.1) THEN
         JTCH = IT1
         IR = IR1
         X(1) = IT1
         X(2) = V1
         X(3) = W1
      ELSE IF (J.EQ.2) THEN
         JTCH = IT2
         IR = IR2
         X(1) = IT2
         X(2) = V2
         X(3) = W2
      ELSE
         IERR = -1
         JTCH = X(1) + 0.001
         IR = 0
         X(2) = 0.0
         X(3) = 0.0
         END IF
C
 999  RETURN
      END
      SUBROUTINE BPFLAG (OP, FOP, IAN, TYPE, FLCHAN, FLFLUX, NFL, IERR)
C-----------------------------------------------------------------------
C   flag samples in specified range, remove them from plot and toplot,
C   put them on flagged plot (if any).  Can also unflag data in core.
C   Handles FC table for UV data flagging only, not unflagging.
C   Inputs:
C      OP       C*4    'UNFL' => unflag, else flag
C                      'FORC' make a new flag table entry even if
C                      already flagged.
C      FOP      I      Type of flag - 1 chan, 2 chanrange, 3 below,
C                      4 above, 5 area, 6 point; < 0 => do NOT raise
C                      the FC number
C      IAN      I      Antenna to flag (if not ALLANT), main antenna in
C                      plots in any case
C      TYPE     I      Data type to check value range
C      FLCHAN   I(2)   Channel indices of flag window (only 1 IF)
C                         that IF is IFNOW
C      FLFLUX   R(2)   Value range to flag
C   Output:
C      NFL      I      Number points flagged/unflagged
C      IERR     I      Error code: 0 -> okay even if no points flagged
C   POLNOW and IFNOW must have real values before calling this routine.
C   The values of 0 => all are not handled inside BPFLAG.
C   2009-01-31: changed to require below and above to send in their
C   samples one at a time like area.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   FOP, IAN, TYPE, FLCHAN(2), NFL, IERR
      REAL      FLFLUX(2)
C
      LONGINT   LPTR, IPTR, JPTR, WPTR
      INTEGER   LR, LR1, LR2, LA, LT, LF, LF1, LF2, LF0, IOP, ITP,
     *   IROUND, I, JJPT, JJPT2, TIME(3), DATE(3), LPP, LPP1, LPP2,
     *   NSORL, SORL(100), ISORL
      CHARACTER FLGOPS(6)*8, STFLAG(3)*4, TTIME(2)*12
      LOGICAL   DOIT, BPOANT, BLNKOK, FORCE
      REAL      V, SGN, PRFLUX(2)
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'EDIFCPS.INC'
      DATA FLGOPS / 'CHANNEL', 'CHNRANGE', 'BELOW', 'ABOVE', 'AREA',
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
C                                       set general parameters
      IF (FLCHAN(1).EQ.FLCHAN(2)) THEN
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
C                                       Do FC table
      IF (ALLTIM) THEN
         FLGIT(1) = 1
         FLGIT(2) = TIMEM
      ELSE
         FLGIT(1) = TIMEC
         FLGIT(2) = TIMEC
         END IF
      IF (DOIT) THEN
C                                       open table for write
         CALL OFCINI (FCFILE, 'WRIT', FLGNUM, FCROW, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (FOP.GE.0) FLGNUM = FLGNUM + 1
         FLGNMX = FLGNUM
         FCROW = FCROW + 1
         FLGOP = FLGOPS(IOP)
         FLGIF(1) = IFNOW
         FLGIF(2) = IFNOW
         FLGANT(2) = ALLANT
         FLGSOR = 0
         FLGCHN(1) = FLCHAN(1)
         FLGCHN(2) = FLCHAN(2)
         FLGSUB = SUBARR
         FLGFQ = FRQSEL
         CALL BPTIMX (FLGIT, DTIMES)
         DFLUXS(1) = FLFLUX(1)
         DFLUXS(2) = FLFLUX(2)
         LDTYPE = DTYPE(LTYPE)(:8)
         FLGREA = REASON
         IF (FLGREA.EQ.' ') THEN
            CALL ZTIME (TIME)
            CALL ZDATE (DATE)
            DATE(1) = -DATE(1)
            CALL TIMDAT (TIME, DATE, TTIME(2), TTIME)
            FLGREA = TSKNAM // TTIME(1)(:9) // ' ' // TTIME(2)(:8)
            END IF
C                                       can do it with one write
         FLGANT(1) = IAN
         IF (ALLANT.EQ.0) FLGANT(1) = 0
         FLGSTK = STFLAG(POLNOW+1)
         IF ((ALLPOL) .OR. (POLMAX.EQ.1)) FLGSTK = STFLAG(1)
         CALL BPTIMX (FLGIT, FLGTIM)
C                                       Source id
         IF (ALLSOR) THEN
            NSORL = 1
            SORL(1) = 0
         ELSE
            NSORL = 100
            CALL BPSORL (FLGIT, NSORL, SORL)
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
      LR1 = EDCORI(PPTR+FLGIT(1))
      IF (FLGIT(2).GE.TIMEM) THEN
         LR2 = MAXREC
      ELSE
         LR2 = EDCORI(PPTR+FLGIT(2)+1) - 1
         END IF
      LR2 = MIN (LR2, MAXREC)
      IF (DIMIF.GT.1) THEN
         LF0 = BIF
      ELSE
         LF0 = MAX (BIF, IFNOW)
         END IF
      IF (LR2.GE.LR1) THEN
         LF1 = (IFNOW-BIF) * CHNMAX + FLCHAN(1)
         LF2 = (IFNOW-BIF) * CHNMAX + FLCHAN(2)
         DO 50 LPP = LPP1,LPP2
            IPTR = DIMPRM + (LPP-1) * DIMIF * DIMDAT
            DO 30 LR = LR1,LR2
               LPTR = DPTR + (LR - 1) * DIMREC
               LA = EDCORI(LPTR+1)
               LT = EDCORI(LPTR+2)
               IF (BPOANT (ALLANT, IAN, LA)) THEN
                  IF ((FLGIT(1).LE.LT) .AND. (FLGIT(2).GE.LT)) THEN
                     DO 25 LF = LF1,LF2
                        WPTR = IPTR + 1 + NUMVAL*(LF-1)
                        JPTR = IPTR + TYPE  + 1 + NUMVAL*(LF-1)
                        JJPT = IPTR + LTYPE + 1 + NUMVAL*(LF-1)
                        IF (DOCOMP) JJPT2 = IPTR + LTYPE2 + 1 +
     *                     NUMVAL*(LF-1)
                        IF ((SGN*EDCORE(LPTR+WPTR).LT.0.0) .OR.
     *                     ((FORCE) .AND. EDCORE(LPTR+WPTR).LT.0)) THEN
                           V = EDCORE(LPTR+JPTR)
C                                       flag it (no flux test)
                           IF ((V.NE.FBLANK) .OR. (BLNKOK)) THEN
                              NFL = NFL + 1
                              EDCORE(LPTR+WPTR) = ABS(EDCORE(LPTR+WPTR))
     *                           * SGN
C                                       change display
                              IF ((PLTAN(1).EQ.LA) .AND. (TIMEC.EQ.LT)
     *                           .AND. (PLTPOL.EQ.LPP)) THEN
C                                       main
                                 V = EDCORE(LPTR+JJPT)
                                 IF (GRSEL(4).GT.0) THEN
                                    ITP = IROUND (SGN * GRSEL(4))
                                    CALL BPPLTP (XYPLOT, ITP, LF, CHAN1,
     *                                 CHAN2, V, APIXR, IERR)
                                    IF (IERR.GT.0) GO TO 980
                                    END IF
C                                       flagged
                                 IF (GRSEL(5).GT.0) THEN
                                    ITP = - IROUND (SGN * GRSEL(5))
                                    CALL BPPLTP (XYPLOT, ITP, LF, CHAN1,
     *                                 CHAN2, V, APIXR, IERR)
                                    IF (IERR.GT.0) GO TO 980
                                    END IF
C                                       Comparison
                                 IF (DOCOMP) V = EDCORE(LPTR+JJPT2)
                                 IF ((GRSEL(4).GT.0) .AND. (DOCOMP))
     *                              THEN
                                    ITP = IROUND (SGN * GRSEL(4))
                                    CALL BPPLTP (EXPLOT, ITP, LF, CHAN1,
     *                                 CHAN2, V, APIXR2, IERR)
                                    IF (IERR.GT.0) GO TO 980
                                    END IF
C                                       flagged comparison
                                 IF ((GRSEL(5).GT.0) .AND. (DOCOMP))
     *                              THEN
                                    ITP = - IROUND (SGN * GRSEL(5))
                                    CALL BPPLTP (EXPLOT, ITP, LF, CHAN1,
     *                                 CHAN2, V, APIXR2, IERR)
                                    IF (IERR.GT.0) GO TO 980
                                    END IF
                                 END IF
                              IF ((GRSEL(6).GT.0) .AND. (PLTPOL.EQ.LPP)
     *                           .AND. (LT.EQ.TIMEC)) THEN
                                 DO 20 I = 2,NUMPLT
                                    IF (PLTAN(I).EQ.LA) THEN
                                       ITP = IROUND (SGN * GRSEL(6))
                                       V = EDCORE(LPTR+JJPT)
                                       CALL BPPLTP (XYPLOT(1,I), ITP,
     *                                    LF, CHAN1, CHAN2, V,
     *                                    APIXR(1,I), IERR)
                                       IF (IERR.GT.0) GO TO 980
                                       IF (GRSEL(5).GT.0) THEN
                                          ITP = - IROUND (SGN*GRSEL(5))
                                          CALL BPPLTP (XYPLOT(1,I), ITP,
     *                                       LF, CHAN1, CHAN2, V,
     *                                       APIXR(1,I), IERR)
                                          IF (IERR.GT.0) GO TO 980
                                          END IF
                                       END IF
 20                                 CONTINUE
                                 END IF
                              END IF
                           END IF
 25                     CONTINUE
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
            WRITE (MSGTXT,1100) 'Flagged', NFL, FLCHAN, PRFLUX
            IF (IOP.LT.3) CALL MSGWRT (2)
         ELSE
            WRITE (MSGTXT,1100) 'Restored', NFL, FLCHAN, PRFLUX
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
 980  MSGTXT = 'BPFLAG: ERROR UPDATING THE TV DISPLAY FOR FLAGGED DATA'
      CALL MSGWRT (6)
      GO TO 999
 990  MSGTXT = 'BPFLAG: ERROR UPDATING THE FLAG COMMAND TABLE'
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' points in CxF',2I7,' x',2(1PE10.2))
      END
      SUBROUTINE BPFLAI (TYPE, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX,
     *   TVBUTT, IERR)
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
C      TVBUTT   I      TV button that was pressed
C      IERR     I      Error code:0 okay, -1 no point
C-----------------------------------------------------------------------
      INTEGER   TYPE, CORN(4), FIRST, TVCO(4), FLCHNS(2), TVBUTT, IERR
      REAL      TVXY(2), FLFLUX(2)
C
      INTEGER   TLC(5), NLINE, ILINE, IP, IX, I, ITRIM, LWIN(4), IT, IR,
     *   TCORN(4), FMODE, WTYPE, JF, JIF
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
         CALL BPDBOX (TVNAME, TYPE, 1, GRSEL(3), TVCO, IERR)
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
         CALL BPDBOX (TVNAME, TYPE, 3, GRSEL(3), TVCO, IERR)
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
         CALL BPDBOX (TVNAME, TYPE, 1, GRSEL(3), TVCO, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       label
         X(1) = (CHAN2 - CHAN1) * (TVXY(1) - XYPLOT(1,1) - LEDG)
     *      / REAL (XYPLOT(3,1) - XYPLOT(1,1) - 2*LEDG) + CHAN1
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
            CALL BPFIND (FMODE, WTYPE, PLTAN(1), LTYPE, TVXY, X, IT, IR,
     *         IERR)
            IF (IERR.GT.0) GO TO 999
            END IF
C                                       use cursor vertical some modes
         IF ((TYPE.GE.2) .AND. (TYPE.LE.4)) X(2) =
     *      (APIXR(2,1)-APIXR(1,1)) * (TVXY(2)-XYPLOT(2,1)-LEDG) /
     *      REAL (XYPLOT(4,1) - XYPLOT(2,1) - 2*LEDG) + APIXR(1,1)
         X(2) = X(2) / DPLSCL(LTYPE)
         DO 110 I = ILINE,NLINE
            IF (I.EQ.1) THEN
               IX = EDCORI(TIMEC+SUPTR)
               IF ((IX.GE.1) .AND. (IX.LE.NSRC)) THEN
                  STRING = SLIST(IX)
               ELSE
                  STRING = ' '
                  END IF
            ELSE IF (I.EQ.2) THEN
               IX = X(1) + 0.5
               JF = MOD (IX-1, CHNMAX) + 1
               JIF = (IX-1) / CHNMAX + BIF
               WRITE (STRING,1101) JF, JIF
            ELSE IF (LTYPE.EQ.1) THEN
               WRITE (STRING,1102) X(2)
            ELSE IF (LTYPE.EQ.2) THEN
               WRITE (STRING,1103) X(2)
            ELSE IF (LTYPE.EQ.3) THEN
               WRITE (STRING,1104) X(2)
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
            CALL BPFIND (1, 0, PLTAN(1), LTYPE, TVXY, X, IT, IR, IERR)
            IF (IERR.GT.0) GO TO 999
            IF (IERR.EQ.0) THEN
               FLCHNS(1) = IT
               CALL BPFIND (2, 0, PLTAN(1), LTYPE, TVXY, X, IT, IR,
     *            IERR)
               IF (IERR.GT.0) GO TO 999
               IERR = 0
               FLCHNS(2) = IT
            ELSE
               CALL BPFIND (3, 0, PLTAN(1), LTYPE, TVXY, X, IT, IR,
     *            IERR)
               IF (IERR.GT.0) GO TO 999
               IERR = 0
               FLCHNS(1) = IT
               FLCHNS(2) = IT
               END IF
         ELSE IF ((TYPE.EQ.5) .OR. (TYPE.EQ.6)) THEN
            TVXY(1) = TVCO(1)
            TVXY(2) = TVCO(2)
            CALL BPFIND (0, 1, PLTAN(1), LTYPE, TVXY, X, IT, IR, IERR)
            IF (IERR.GT.0) GO TO 999
            FLCHNS(1) = IT
            FLCHNS(2) = IT
            FLFLUX(1) = X(2)
            FLFLUX(2) = X(2)
         ELSE
            TVXY(1) = TVCO(1)
            CALL BPFIND (4, 0, PLTAN(1), LTYPE, TVXY, X, IT, IR, IERR)
            IF (IERR.GT.0) GO TO 999
            FLCHNS(1) = IT
            TVXY(1) = TVCO(3)
            CALL BPFIND (5, 0, PLTAN(1), LTYPE, TVXY, X, IT, IR, IERR)
            IF (IERR.GT.0) GO TO 999
            FLCHNS(2) = IT
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
 980  MSGTXT = 'BPFLAI: ERROR SETTING FLAG WINDOW'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1101 FORMAT ('CHIF',I7,'/',I4)
 1102 FORMAT ('AMPLI',F7.3)
 1103 FORMAT ('PHASE',F7.1)
 1104 FORMAT ('RESID',F7.1)
      END
      SUBROUTINE BPFLAR (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of an area: channel range and flux range
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLCHNS(2), TVCO(4), NFL, FOP,
     *   IFL, POLOLD, IFOLD, IP1, IP2, FLCHAN(2), FLCHNT(2)
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
      CALL BPFLAI (4, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         FOP = 5
         IFL = 0
         PLTPOL = POLOLD
C                                       Loop for each point
C                                       individually
         DO 30 POLNOW = IP1,IP2
            FLCHNT(1) = FLCHNS(1)
            FLCHNT(2) = FLCHNS(2)
 20         CALL BPGTAP (PLTAN(1), LTYPE, FLCHNT, FLFLUX, IERR)
            IF (IERR.GT.0) GO TO 999
            IF (IERR.EQ.0) THEN
               IFNOW = (FLCHNT(1) - 1) / CHNMAX + BIF
               FLCHAN(1) = MOD (FLCHNT(1) - 1, CHNMAX) + 1
               FLCHAN(2) = FLCHAN(1)
               CALL BPFLAG ('FORC', FOP, PLTAN(1), LTYPE, FLCHAN,
     *            FLFLUX, NFL, IERR)
               IF (IERR.GT.0) GO TO 980
               IFL = IFL + NFL
               IF (NFL.GT.0) FOP = -5
               FLCHNT(1) = FLCHNT(1) + 1
               IF (FLCHNT(1).LE.FLCHNT(2)) GO TO 20
               END IF
 30         CONTINUE
         PRFLUX(1) = FLFLUX(1) / DPLSCL(LTYPE)
         PRFLUX(2) = FLFLUX(2) / DPLSCL(LTYPE)
         WRITE (MSGTXT,1100) 'Flagged', IFL, FLCHNS, PRFLUX
         CALL MSGWRT (2)
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL BPFLAI (4, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'BPFLAR: ERROR DOING AREA FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' points in CxF',2I7,' x',2(1PE10.2))
      END
      SUBROUTINE BPFLFA (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of high fluxes
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLCHNS(2), TVCO(4), NFL, IFL,
     *   FOP, POLOLD, IFOLD, IP1, IP2, FLCHAN(2), FLCHNT(2)
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
      CALL BPFLAI (3, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         FLFLUX(2) = 1.E20
         FOP = 4
         IFL = 0
         PLTPOL = POLOLD
C                                       Loop for each point
C                                       individually
         DO 30 POLNOW = IP1,IP2
            FLCHNT(1) = FLCHNS(1)
            FLCHNT(2) = FLCHNS(2)
 20         CALL BPGTAP (PLTAN(1), LTYPE, FLCHNT, FLFLUX, IERR)
            IF (IERR.GT.0) GO TO 999
            IF (IERR.EQ.0) THEN
               IFNOW = (FLCHNT(1) - 1) / CHNMAX + BIF
               FLCHAN(1) = MOD (FLCHNT(1) - 1, CHNMAX) + 1
               FLCHAN(2) = FLCHAN(1)
               CALL BPFLAG ('FORC', FOP, PLTAN(1), LTYPE, FLCHAN,
     *            FLFLUX, NFL, IERR)
               IF (IERR.GT.0) GO TO 980
               IFL = IFL + NFL
               IF (NFL.GT.0) FOP = -4
               FLCHNT(1) = FLCHNT(1) + 1
               IF (FLCHNT(1).LE.FLCHNT(2)) GO TO 20
               END IF
 30         CONTINUE
         PRFLUX(1) = FLFLUX(1) / DPLSCL(LTYPE)
         WRITE (MSGTXT,1100) 'Flagged', IFL, FLCHNS, PRFLUX(1)
         CALL MSGWRT (2)
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL BPFLAI (3, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'BPFLFA: ERROR DOING FLUX ABOVE FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' points in C',2I7,'  F >',1PE11.3)
      END
      SUBROUTINE BPFLFB (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of low fluxes
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLCHNS(2), TVCO(4), NFL, IFL,
     *   FOP, POLOLD, IFOLD, IP1, IP2, FLCHAN(2), FLCHNT(2)
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
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         MSGTXT = 'Hit buttons A or B to flag low fluxes and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to flag low fluxes and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
C                                       read cursor until button
      CALL BPFLAI (2, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         FLFLUX(1) = -1.E20
         FOP = 3
         IFL = 0
         PLTPOL = POLOLD
C                                       Loop for each point
C                                       individually
         DO 30 POLNOW = IP1,IP2
            FLCHNT(1) = FLCHNS(1)
            FLCHNT(2) = FLCHNS(2)
 20         CALL BPGTAP (PLTAN(1), LTYPE, FLCHNT, FLFLUX, IERR)
            IF (IERR.GT.0) GO TO 999
            IF (IERR.EQ.0) THEN
               IFNOW = (FLCHNT(1) - 1) / CHNMAX + BIF
               FLCHAN(1) = MOD (FLCHNT(1) - 1, CHNMAX) + 1
               FLCHAN(2) = FLCHAN(1)
               CALL BPFLAG ('FORC', FOP, PLTAN(1), LTYPE, FLCHAN,
     *            FLFLUX, NFL, IERR)
               IF (IERR.GT.0) GO TO 980
               IFL = IFL + NFL
               IF (NFL.GT.0) FOP = -3
               FLCHNT(1) = FLCHNT(1) + 1
               IF (FLCHNT(1).LE.FLCHNT(2)) GO TO 20
               END IF
 30         CONTINUE
         PRFLUX(2) = FLFLUX(2) / DPLSCL(LTYPE)
         WRITE (MSGTXT,1100) 'Flagged', IFL, FLCHNS, PRFLUX(2)
         CALL MSGWRT (2)
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL BPFLAI (2, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'BPFLFB: ERROR DOING FLUX BERLOW FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' points in C',2I7,'   F <',1PE11.3)
      END
      SUBROUTINE BPFLPT (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of points (uses Y and X position)
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLCHNS(2), TVCO(4), NFL, NM,
     *   POLOLD, IFOLD, FLCHAN(2), FOP, IP1, IP2, LF1, LF2, IFL
      REAL      TVXY(2), FLFLUX(2)
      LOGICAL   DOMSG
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      NM = 0
      POLOLD = POLNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      DOMSG = .TRUE.
      IFL = 0
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
      CALL BPFLAI (5, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.GT.0) GO TO 980
      PLTPOL = POLOLD
C                                       flag something
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7)) THEN
         IF (IERR.EQ.0) THEN
            FOP = 6
            DO 30 POLNOW = IP1,IP2
               LF1 = (FLCHNS(1) - 1) / CHNMAX + BIF
               LF2 = (FLCHNS(2) - 1) / CHNMAX + BIF
               FLCHAN(1) = MOD (FLCHNS(1)-1, CHNMAX) + 1
               DO 20 IFNOW = LF1,LF2
                  IF (IFNOW.EQ.LF2) THEN
                     FLCHAN(2) = MOD (FLCHNS(2)-1, CHNMAX) + 1
                  ELSE
                     FLCHAN(2) = CHNMAX
                     END IF
                  CALL BPFLAG ('FLAG', FOP, PLTAN(1), LTYPE, FLCHAN,
     *               FLFLUX, NFL, IERR)
                  IF (IERR.GT.0) GO TO 980
                  FLCHAN(1) = 1
                  IF (NFL.GT.0) FOP = -6
                  IFL = IFL + NFL
 20               CONTINUE
 30            CONTINUE
         ELSE
            MSGTXT = 'No unflagged sample in the vicinity'
            IF (DOMSG) CALL MSGWRT (1)
            DOMSG = .FALSE.
            END IF
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
      WRITE (MSGTXT,1030) IFL
      CALL MSGWRT (2)
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL BPFLAI (5, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'BPFLPT: ERROR DOING POINT FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Flagged',I6,' points')
      END
      SUBROUTINE BPFLQU (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of points in quick mode
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLCHNS(2), TVCO(4), NFL, IFOLD,
     *   NM, FLCHAN(2), LF1, LF2, IFL
      REAL      TVXY(2), FLFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      IFOLD = IFNOW
      NM = 0
      IFL = 0
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
      IFNOW = IFOLD
      CALL BPFLAI (6, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      IF ((TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         LF1 = (FLCHNS(1) - 1) / CHNMAX + BIF
         LF2 = (FLCHNS(2) - 1) / CHNMAX + BIF
         FLCHAN(1) = MOD (FLCHNS(1)-1, CHNMAX) + 1
         DO 20 IFNOW = LF1,LF2
            IF (IFNOW.EQ.LF2) THEN
               FLCHAN(2) = MOD (FLCHNS(2)-1, CHNMAX) + 1
            ELSE
               FLCHAN(2) = CHNMAX
               END IF
            CALL BPFLAG ('FLAG', 6, PLTAN(1), LTYPE, FLCHAN, FLFLUX,
     *         NFL, IERR)
            IF (IERR.GT.0) GO TO 980
            IFL = IFL + NFL
            FLCHAN(1) = 1
 20         CONTINUE
         IF (NM.NE.0) NM = MOD (NM+1, 20)
         END IF
C                                       loop
      IF (TVBUTT.LE.0) GO TO 10
      WRITE (MSGTXT,1020) IFL
      CALL MSGWRT (2)
C                                       clear
      IFNOW = IFOLD
      FIRST = -1
      CALL BPFLAI (6, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'BPFLQU: ERROR DOING FAST POINT FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Flagged',I6,' points')
      END
      SUBROUTINE BPFLCI (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of a single channel
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLCHNS(2), TVCO(4), NFL, POLOLD,
     *   IFOLD, IP1, IP2, FOP, NM, LF1, LF2, FLCHAN(2)
      REAL      TVXY(2), FLFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      NM = 0
      FIRST = 1
      POLOLD = POLNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLOLD
         IP2 = POLOLD
         END IF
C                                       instructions
 10   IF ((TVBUTT.GT.0) .AND. (NM.EQ.0)) THEN
         MSGTXT = 'Hit buttons A or B to flag channel and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to flag channel and return to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
      NM = MOD (NM+1, 7)
C                                       read cursor until button
      POLNOW = POLOLD
      CALL BPFLAI (0, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      FLFLUX(1) = -1.E20
      FLFLUX(2) = 1.E20
      FOP = 1
      PLTPOL = POLOLD
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         DO 30 POLNOW = IP1,IP2
            LF1 = (FLCHNS(1) - 1) / CHNMAX + BIF
            LF2 = (FLCHNS(2) - 1) / CHNMAX + BIF
            FLCHAN(1) = MOD (FLCHNS(1)-1, CHNMAX) + 1
            DO 20 IFNOW = LF1,LF2
               IF (IFNOW.EQ.LF2) THEN
                  FLCHAN(2) = MOD (FLCHNS(2)-1, CHNMAX) + 1
               ELSE
                  FLCHAN(2) = CHNMAX
               END IF
               CALL BPFLAG ('FLAG', FOP, PLTAN(1), LTYPE, FLCHAN,
     *            FLFLUX, NFL, IERR)
               IF (IERR.GT.0) GO TO 980
               FLCHAN(1) = 1
 20            CONTINUE
 30         CONTINUE
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL BPFLAI (0, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'BPFLCI: ERROR DOING SINGLE-TIME FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE BPFLCR (IERR)
C-----------------------------------------------------------------------
C   interactive flagging of a channel-range
C   Inputs:
C      IGS    I(4)   Graphics plane of data, extra data, flagged, top
C   Outputs:
C      IERR   I      Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   CORN(4), TVBUTT, FIRST, FLCHNS(2), TVCO(4), NFL, POLOLD,
     *   IFOLD, IP1, IP2, FOP, NM, LF1, LF2, FLCHAN(2)
      REAL      TVXY(2), FLFLUX(2)
      INCLUDE 'EDIUTIL.INC'
C-----------------------------------------------------------------------
      CALL COPY (4, XYPLOT(1,1), CORN)
      TVBUTT = 1
      FIRST = 1
      POLOLD = POLNOW
      IF (POLOLD.EQ.0) THEN
         IP1 = 1
         IP2 = POLMAX
      ELSE
         IP1 = POLNOW
         IP2 = POLNOW
         END IF
      NM = 0
C                                       instructions
 10   IF ((TVBUTT.GT.0) .AND. (NM.EQ.0)) THEN
         MSGTXT = 'Hit button A to set the other side of the' //
     *      ' channel range'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to flag channel range and continue'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to flag channel range and return' //
     *      ' to menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu with no more flagging'
         CALL MSGWRT (1)
         END IF
      NM = MOD (NM+1, 4)
C                                       read cursor until button
      POLNOW = POLOLD
      CALL BPFLAI (1, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.GT.0) GO TO 980
C                                       flag something
      FLFLUX(1) = -1.E20
      FLFLUX(2) = 1.E20
      FOP = 2
      PLTPOL = POLOLD
      IF ((TVBUTT.GE.1) .AND. (TVBUTT.LE.7) .AND. (IERR.EQ.0)) THEN
         DO 30 POLNOW = IP1,IP2
            LF1 = (FLCHNS(1) - 1) / CHNMAX + BIF
            LF2 = (FLCHNS(2) - 1) / CHNMAX + BIF
            FLCHAN(1) = MOD (FLCHNS(1)-1, CHNMAX) + 1
            DO 20 IFNOW = LF1,LF2
               IF (IFNOW.EQ.LF2) THEN
                  FLCHAN(2) = MOD (FLCHNS(2)-1, CHNMAX) + 1
               ELSE
                  FLCHAN(2) = CHNMAX
               END IF
               CALL BPFLAG ('FLAG', FOP, PLTAN(1), LTYPE, FLCHAN,
     *            FLFLUX, NFL, IERR)
               IF (IERR.GT.0) GO TO 980
               FLCHAN(1) = 1
 20            CONTINUE
 30         CONTINUE
         END IF
C                                       loop
      IF (TVBUTT.LE.3) GO TO 10
C                                       clear
      POLNOW = POLOLD
      IFNOW = IFOLD
      FIRST = -1
      CALL BPFLAI (1, CORN, FIRST, TVXY, TVCO, FLCHNS, FLFLUX, TVBUTT,
     *   IERR)
      IF (IERR.LE.0) GO TO 999
C
 980  MSGTXT = 'BPFLCR: ERROR DOING TIME-RANGE FLAGS'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE BPFRAM (IERR)
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
      CALL BPTWIN ('ON', 0, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       instructions
 10   IF (TVBUTT.GT.0) THEN
         IF (LOWER) THEN
            TVBUTT = (CHAN1 - 1.0) / (CHNTOT - 1.0) * (CORN(3) - CORN(1)
     *         - 2*LEDG) + 0.5 + CORN(1) + LEDG
            MSGTXT = 'Setting lower limit with cursor X'
            CALL MSGWRT (1)
            MSGTXT = 'Hit buttons A or B to do upper limit'
         ELSE
            TVBUTT = (CHAN2 - 1.0) / (CHNTOT - 1.0) * (CORN(3) - CORN(1)
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
         CALL BPTWIN ('OFF', 1, IERR)
         IF (IERR.NE.0) GO TO 980
         CHAN1 = (TVXY(1) - LEDG - CORN(1)) * (CHNTOT - 1.0) /
     *      (CORN(3) - CORN(1) - 2*LEDG) + 1.5
         CHAN1 = MAX (1, MIN (CHNTOT-1, CHAN1))
         CALL BPTWIN ('ON', 1, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       right line
      ELSE
         CALL BPTWIN ('OFF', 2, IERR)
         IF (IERR.NE.0) GO TO 980
         CHAN2 = (TVXY(1) - LEDG - CORN(1)) * (CHNTOT - 1.0) /
     *      (CORN(3) - CORN(1) - 2*LEDG) + 1.5
         CHAN2 = MAX (1, MIN (CHNTOT, CHAN2))
         CALL BPTWIN ('ON', 2, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      IF (TVBUTT.GT.0) LOWER = .NOT.LOWER
      IF (TVBUTT.LT.4) GO TO 10
      IF (CHAN2.LT.CHAN1) THEN
         TVBUTT = CHAN1
         CHAN1 = CHAN2
         CHAN2 = TVBUTT
         END IF
      CALL BPTWIN ('OFF', 0, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 999
C
 980  MSGTXT = 'BPFRAM: ERROR SETTING FRAME INTERACTIVELY'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE BPGTAP (ANT, TYPE, FLCHNS, FLFLUX, IERR)
C-----------------------------------------------------------------------
C   get the next sample in the flchms, flflux area even if flagged
C   Inputs:
C      ANT      I      Antenna number
C      TYPE     I      Data type
C      FLFLUX   R(2)   Lower, upper flux limits
C   In/Out:
C      FLCHNS   I(2)   in: Start, stop channels of area
C                      out: start changed to point to sample found
C   Output:
C      IERR     I      Error code: < 0 no points found
C   This routine does NOT support POLNOW and IFNOW = 0.  They must be
C   set to real values.
C-----------------------------------------------------------------------
      INTEGER   ANT, TYPE, FLCHNS(2), IERR
      REAL      FLFLUX(2)
C
      LONGINT   LPTR, IPTR, JPTR
      INTEGER   LR1, LR2, LF, LR, LA, LT
      REAL      V
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
      LR1 = EDCORI(PPTR+TIMEC)
      LR2 = EDCORI(PPTR+TIMEC+1) - 1
      LR2 = MIN (LR2, MAXREC)
      IERR = -1
      IF (LR2.GE.LR1) THEN
         IPTR = DIMPRM + (POLNOW-1) * DIMIF * DIMDAT
         DO 20 LR = LR1,LR2
            LPTR = DPTR + (LR - 1) * DIMREC
            LA = EDCORI(LPTR+1)
            LT = EDCORI(LPTR+2)
C                                       Only current baseline
C                                       in time range
            IF ((ANT.EQ.LA) .AND. (TIMEC.EQ.LT)) THEN
               DO 10 LF = FLCHNS(1),FLCHNS(2)
                  JPTR = LPTR + IPTR + NUMVAL*(LF-1)
C                                       ignore weight/flagging
                  V = EDCORE(JPTR+TYPE+1)
C                                       want it
                  IF ((V.NE.FBLANK) .AND. (FLFLUX(1).LE.V) .AND.
     *               (FLFLUX(2).GE.V) .AND. (EDCORE(JPTR+1).NE.0.0))
     *               THEN
                     FLCHNS(1) = LF
                     IERR = 0
                     GO TO 999
                     END IF
 10               CONTINUE
               END IF
 20         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE BPGTBP (IERR)
C-----------------------------------------------------------------------
C   Gets BP data from OBJECT.
C   Output:
C      IERR     I       Error code: > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      LONGINT   LPTR, IPTR, JPTR, KPTR
      INTEGER   I, NCL, ROW1, ROW2, IROW, IAN, TYPE, DIM(7), ITIM, IFQ,
     *   LF, LF1, LF2, IREC, IPOL, IPRM, ICL, SUBA, SOURC,
     *   GRNONE(5), LSORC, KF, LC
      REAL      W, DR, DI, VALUE(MAXCIF), TI
      DOUBLE PRECISION TIME, LTIME
      CHARACTER COL1(3)*10, COL2(3)*10, COLRP(6)*10, CDUMMY*1
      DATA COLRP /'ANTENNA', 'TIME', 'INTERVAL', 'FREQ ID', 'SUBARRAY',
     *   'SOURCE ID'/
      DATA COL1 /'WEIGHT 1', 'REAL 1', 'IMAG 1'/
      DATA COL2 /'WEIGHT 2', 'REAL 2', 'IMAG 2'/
C-----------------------------------------------------------------------
C                                       First time
      IF (MAXREC.LE.0) THEN
         LTIME = -1000.
         LSORC = -10
         IF (DTIME.LE.0.0) DTIME = 1.0 / 6000.0
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0*DTIME)
         DTIME = DTIME / (24 * 60.)
         GTIME = GTIME / (24 * 60.)
C                                       Get column pointers
         DIMPRM = 3
         DIMIF = EIF - BIF + 1
         NUMVAL = 3
         DIMDAT = NUMVAL * CHNMAX
         CHNTOT = CHNMAX * DIMIF
         DIMREC = DIMPRM + DIMDAT * DIMIF * POLMAX
         NCL = 6
         COLLAB(1) = COLRP(1)
         COLLAB(2) = COLRP(2)
         COLLAB(3) = COLRP(3)
         COLLAB(4) = COLRP(4)
         COLLAB(5) = COLRP(5)
         COLLAB(6) = COLRP(6)
         DO 10 I = 1,3
            NCL = NCL + 1
            COLLAB(NCL) = COL1(I)
 10         CONTINUE
         IF (POLMAX.EQ.2) THEN
            DO 15 I = 1,3
               NCL = NCL + 1
               COLLAB(NCL) = COL2(I)
 15            CONTINUE
            END IF
         CALL TABCOL (TBEDIT, NCL, COLLAB, COLNUM, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'BPGTBP FAILS TO GET COLUMN NUMBERS'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       read through file to count rows
C                                       and to study times.
         ROW1 = ROWMAX + 1
         ROW2 = -1
         MAXREC = 0
         TIMEM = 1
         DO 30 IROW = 1,ROWMAX
            IREC = IROW
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (IERR.LT.0) GO TO 30
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 30
            ICL = 5
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SUBA,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((SUBA.GT.0) .AND. (SUBA.NE.SUBARR)) GO TO 30
            ICL = 6
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SOURC,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
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
            IF ((TIME-LTIME.GT.DTIME) .OR. (SOURC.NE.LSORC)) THEN
               TIMEM = TIMEM + 1
               LTIME = TIME
               LSORC = SOURC
               END IF
 30         CONTINUE
         IERR = 0
C                                       How big is the problem?
 40      TIMEM = TIMEM + 2
         I = MAXREC * DIMREC + 4 * TIMEM
         EDSIZE = I + 2048
         EDSIZE = (EDSIZE - 1) / 1024 + 1
         EDIPTR = 0
         CALL ZMEMRY ('GET ', 'BPGTBP', EDSIZE, EDCORE, EDIPTR, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'UNABLE TO ALLOCATE NEEDED MEMORY'
            CALL MSGWRT (8)
            MSGTXT = 'LIMIT IFS WITH BIF AND EIF AND TRY AGAIN'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         APOPEN = .TRUE.
         EDSIZE = EDSIZE * 1024
         WRITE (MSGTXT,4000) I, EDIPTR
         CALL MSGWRT (9)
         MSGTXT = 'All data will reside in memory'
         CALL MSGWRT (3)
C                                       now fill in times, ants, ..
         TIPTR = EDIPTR + TIMEM
         SUPTR = TIPTR + TIMEM
         PPTR = SUPTR + TIMEM
         DPTR = PPTR + TIMEM
         LPTR = DPTR
         IPTR = PPTR
         JPTR = SUPTR
         KPTR = TIPTR
         TIMEM = 0
         LTIME = -1000
         LSORC = -1
         ITIM = 0
         MAXREC = 0
         DO 60 IROW = ROW1,ROW2
            IREC = IROW
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (IERR.LT.0) GO TO 60
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 60
            ICL = 5
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
            ICL = 6
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SOURC,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            SOURC = MAX (1, SOURC)
            ICL = 3
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, TI,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((TIME-LTIME.GT.DTIME) .OR. (LSORC.NE.SOURC)) THEN
               IF (TIMEM.EQ.0) THEN
                  TIMEM = 1
                  TIMES(EDIPTR+TIMEM) = TIME - 2*TI
                  IPTR = IPTR + 1
                  EDCORI(IPTR) = MAXREC + 1
                  JPTR = JPTR + 1
                  EDCORI(JPTR) = SOURC
                  KPTR = KPTR + 1
                  EDCORE(KPTR) = TI
                  END IF
               TIMEM = TIMEM + 1
               TIMES(EDIPTR+TIMEM) = TIME
               LTIME = TIME
               LSORC = SOURC
               IPTR = IPTR + 1
               EDCORI(IPTR) = MAXREC + 1
               JPTR = JPTR + 1
               EDCORI(JPTR) = SOURC
               KPTR = KPTR + 1
               EDCORE(KPTR) = TI
               END IF
C                                       basic pointers
            EDCORI(LPTR+3) = IROW
            EDCORI(LPTR+1) = IAN
            EDCORI(LPTR+2) = TIMEM
            MAXREC = MAXREC + 1
            LPTR = LPTR + DIMREC
            MSAMPS(IAN) = MSAMPS(IAN) + 1
 60         CONTINUE
         IERR = 0
C                                       finish the times
 70      TIMEM = TIMEM + 1
         TIMES(EDIPTR+TIMEM) = TIME + 2 * TI
         IPTR = IPTR + 1
         EDCORI(IPTR) = MAXREC + 1
         JPTR = JPTR + 1
         EDCORI(JPTR) = SOURC
         KPTR = KPTR + 1
         EDCORE(KPTR) = TI
         TIMEM = TIMEM + 1
         TIMES(EDIPTR+TIMEM) = TIME + 6 * TI
         IPTR = IPTR + 1
         EDCORI(IPTR) = MAXREC + 1
         JPTR = JPTR + 1
         EDCORI(JPTR) = SOURC
         KPTR = KPTR + 1
         EDCORE(KPTR) = TI
         WRITE (MSGTXT,1070) TIMEM-3, MAXREC
         CALL MSGWRT (3)
         END IF
C                                       if allin and in the middle,
C                                       there is nothing to do
      IF ((PIFNOW.LT.0) .OR. (IFNOW.LT.0)) THEN
         LF1 = BIF
         LF2 = EIF
         LPTR = DPTR
         DO 190 IREC = 1,MAXREC
            IROW = EDCORI(LPTR+3)
            DO 150 IPOL = 1,POLMAX
C                                       weight
               ICL = 7 + 3 * (IPOL-1)
               CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, VALUE,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (IERR.LT.0) GO TO 185
               IF (IFNOW.GE.0) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
                  DO 110 LF = LF1,LF2
                     DO 105 LC = 1,CHNMAX
                        EDCORE(JPTR+1) = VALUE(LF)
                        JPTR = JPTR + NUMVAL
 105                    CONTINUE
 110                 CONTINUE
                  END IF
C                                       REAL/IMAG
               DO 125 IPRM = 2,3
                  ICL = ICL + 1
                  CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *               VALUE, CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  IF (IERR.LT.0) GO TO 185
                  IF (IFNOW.GE.0) THEN
                     JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
                     DO 120 LF = LF1,LF2
                        KF = (LF - 1) * CHNMAX + 1
                        DO 115 LC = 1,CHNMAX
                           EDCORE(JPTR+IPRM) = VALUE(KF)
                           JPTR = JPTR + NUMVAL
                           KF = KF + 1
 115                       CONTINUE
 120                    CONTINUE
                     END IF
 125              CONTINUE
C                                       real/imag -> amp/phase
               IF (IFNOW.GE.0) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
                  DO 140 LF = LF1,LF2
                     DO 135 LC = 1,CHNMAX
                        W = EDCORE(JPTR+1)
                        DR = EDCORE(JPTR+2)
                        DI = EDCORE(JPTR+3)
                        IF ((DR.EQ.FBLANK) .OR. (DI.EQ.FBLANK) .OR.
     *                     ((DR.EQ.0.0) .AND. (DI.EQ.0.0))) THEN
                           EDCORE(JPTR+1) = -ABS(W)
                        ELSE
                           EDCORE(JPTR+2) = SQRT (DR*DR + DI*DI)
                           EDCORE(JPTR+3) = ATAN2 (DI, DR) /DG2RAD
                           END IF
                        JPTR = JPTR + NUMVAL
 135                    CONTINUE
 140                 CONTINUE
                  END IF
 150           CONTINUE
 185        LPTR = LPTR + DIMREC
 190        CONTINUE
         WASFLG = -1
         IF (IFNOW.GE.0) THEN
            CALL BPFGAP (UVMAST, IERR)
            IF (IERR.EQ.0) FLAGED = .TRUE.
            END IF
         END IF
C                                       redo flags
      IF (IFNOW.GT.0) THEN
         CALL COPY (5, GRSEL(4), GRNONE)
         CALL FILL (5, 0, GRSEL(4))
         CALL BPFCDO (IERR)
         CALL COPY (5, GRNONE, GRSEL(4))
         END IF
      GO TO 999
C                                       write error
 990  WRITE (MSGTXT,1990) IREC, IROW, COLLAB(ICL)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('Found',I4,' times with',I7,' spectra total')
 1990 FORMAT ('BPGTBP: ERROR READING REC',I5,' ROW',I6,' COL ',A)
 4000 FORMAT ('Allocate ',I12,' words at',I18)
      END
      SUBROUTINE PCGTPC (IERR)
C-----------------------------------------------------------------------
C   Gets PC data from OBJECT.
C   Output:
C      IERR     I       Error code: > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PPCV.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INTEGER   MAXV
      PARAMETER (MAXV = 2*MAXTON*MAXIF)
C
      LONGINT   LPTR, IPTR, JPTR, KPTR
      INTEGER   I, NCL, ROW1, ROW2, IROW, IAN, TYPE, DIM(7), ITIM, IFQ,
     *   LF, LF1, LF2, IREC, IPOL, IPRM, ICL, SUBA, SOURC,
     *   GRNONE(5), LSORC, KF, LC
      REAL      W, DR, DI, VALUE(MAXV), TI, RTEMP(2)
      DOUBLE PRECISION TIME, LTIME, DVALUE(MAXV), DTEMP
      CHARACTER COL1(3)*12, COL2(3)*12, COLRP(6)*24, CDUMMY*1
      EQUIVALENCE (VALUE, DVALUE), (RTEMP, DTEMP)
      DATA COLRP /'ANTENNA_NO', 'TIME', 'TIME_INTERVAL', 'FREQID',
     *   'ARRAY', 'SOURCE_ID'/
      DATA COL1 /'PC_REAL 1', 'PC_IMAG 1', 'PC_FREQ 1'/
      DATA COL2 /'PC_REAL 2', 'PC_IMAG 2', 'PC_FREQ 2'/
C-----------------------------------------------------------------------
C                                       First time
      IF (MAXREC.LE.0) THEN
         LTIME = -1000.
         LSORC = -10
         IF (DTIME.LE.0.0) DTIME = 1.0 / 6000.0
         IF (GTIME.LE.DTIME) GTIME = MAX (2.0, 5.0*DTIME)
         DTIME = DTIME / (24 * 60.)
         GTIME = GTIME / (24 * 60.)
C                                       Get column pointers
         DIMPRM = 3
         DIMIF = EIF - BIF + 1
         NUMVAL = 6
         DIMDAT = NUMVAL * CHNMAX
         CHNTOT = CHNMAX * DIMIF
         DIMREC = DIMPRM + DIMDAT * DIMIF * POLMAX
         NCL = 6
         COLLAB(1) = COLRP(1)
         COLLAB(2) = COLRP(2)
         COLLAB(3) = COLRP(3)
         COLLAB(4) = COLRP(4)
         COLLAB(5) = COLRP(5)
         COLLAB(6) = COLRP(6)
         DO 10 I = 1,3
            NCL = NCL + 1
            COLLAB(NCL) = COL1(I)
 10         CONTINUE
         IF (POLMAX.EQ.2) THEN
            DO 15 I = 1,3
               NCL = NCL + 1
               COLLAB(NCL) = COL2(I)
 15            CONTINUE
            END IF
         CALL TABCOL (TBEDIT, NCL, COLLAB, COLNUM, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'PCGTPC FAILS TO GET COLUMN NUMBERS'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       read through file to count rows
C                                       and to study times.
         ROW1 = ROWMAX + 1
         ROW2 = -1
         MAXREC = 0
         TIMEM = 1
         DO 30 IROW = 1,ROWMAX
            IREC = IROW
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (IERR.LT.0) GO TO 30
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 30
            ICL = 5
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SUBA,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((SUBA.GT.0) .AND. (SUBA.NE.SUBARR)) GO TO 30
            ICL = 6
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SOURC,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
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
            IF ((TIME-LTIME.GT.DTIME) .OR. (SOURC.NE.LSORC)) THEN
               TIMEM = TIMEM + 1
               LTIME = TIME
               LSORC = SOURC
               END IF
 30         CONTINUE
         IERR = 0
C                                       How big is the problem?
 40      TIMEM = TIMEM + 2
         I = MAXREC * DIMREC + 4 * TIMEM
         EDSIZE = I + 2048
         EDSIZE = (EDSIZE - 1) / 1024 + 1
         EDIPTR = 0
         CALL ZMEMRY ('GET ', 'PCGTPC', EDSIZE, EDCORE, EDIPTR, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'UNABLE TO ALLOCATE NEEDED MEMORY'
            CALL MSGWRT (8)
            MSGTXT = 'LIMIT IFS WITH BIF AND EIF AND TRY AGAIN'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         APOPEN = .TRUE.
         EDSIZE = EDSIZE * 1024
         WRITE (MSGTXT,4000) I, EDIPTR
         CALL MSGWRT (9)
         MSGTXT = 'All data will reside in memory'
         CALL MSGWRT (3)
C                                       now fill in times, ants, ..
         TIPTR = EDIPTR + TIMEM
         SUPTR = TIPTR + TIMEM
         PPTR = SUPTR + TIMEM
         DPTR = PPTR + TIMEM
         LPTR = DPTR
         IPTR = PPTR
         JPTR = SUPTR
         KPTR = TIPTR
         TIMEM = 0
         LTIME = -1000
         LSORC = -1
         ITIM = 0
         MAXREC = 0
         DO 60 IROW = ROW1,ROW2
            IREC = IROW
            ICL = 4
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, IFQ,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (IERR.LT.0) GO TO 60
            IF ((IFQ.GT.0) .AND. (FRQSEL.GT.0) .AND. (IFQ.NE.FRQSEL))
     *         GO TO 60
            ICL = 5
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
            ICL = 6
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, SOURC,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            SOURC = MAX (1, SOURC)
            ICL = 3
            CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM, TI,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((TIME-LTIME.GT.DTIME) .OR. (LSORC.NE.SOURC)) THEN
               IF (TIMEM.EQ.0) THEN
                  TIMEM = 1
                  TIMES(EDIPTR+TIMEM) = TIME - 2*TI
                  IPTR = IPTR + 1
                  EDCORI(IPTR) = MAXREC + 1
                  JPTR = JPTR + 1
                  EDCORI(JPTR) = SOURC
                  KPTR = KPTR + 1
                  EDCORE(KPTR) = TI
                  END IF
               TIMEM = TIMEM + 1
               TIMES(EDIPTR+TIMEM) = TIME
               LTIME = TIME
               LSORC = SOURC
               IPTR = IPTR + 1
               EDCORI(IPTR) = MAXREC + 1
               JPTR = JPTR + 1
               EDCORI(JPTR) = SOURC
               KPTR = KPTR + 1
               EDCORE(KPTR) = TI
               END IF
C                                       basic pointers
            EDCORI(LPTR+3) = IROW
            EDCORI(LPTR+1) = IAN
            EDCORI(LPTR+2) = TIMEM
            MAXREC = MAXREC + 1
            LPTR = LPTR + DIMREC
            MSAMPS(IAN) = MSAMPS(IAN) + 1
 60         CONTINUE
         IERR = 0
C                                       finish the times
 70      TIMEM = TIMEM + 1
         TIMES(EDIPTR+TIMEM) = TIME + 2 * TI
         IPTR = IPTR + 1
         EDCORI(IPTR) = MAXREC + 1
         JPTR = JPTR + 1
         EDCORI(JPTR) = SOURC
         KPTR = KPTR + 1
         EDCORE(KPTR) = TI
         TIMEM = TIMEM + 1
         TIMES(EDIPTR+TIMEM) = TIME + 6 * TI
         IPTR = IPTR + 1
         EDCORI(IPTR) = MAXREC + 1
         JPTR = JPTR + 1
         EDCORI(JPTR) = SOURC
         KPTR = KPTR + 1
         EDCORE(KPTR) = TI
         WRITE (MSGTXT,1070) TIMEM-3, MAXREC
         CALL MSGWRT (3)
         END IF
C                                       if allin and in the middle,
C                                       there is nothing to do
      IF ((PIFNOW.LT.0) .OR. (IFNOW.LT.0)) THEN
         LF1 = BIF
         LF2 = EIF
         LPTR = DPTR
         DO 190 IREC = 1,MAXREC
            IROW = EDCORI(LPTR+3)
            DO 150 IPOL = 1,POLMAX
C                                       weight - not in PC table
               ICL = 6 + 3 * (IPOL-1)
               IF (IFNOW.GE.0) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
                  DO 110 LF = LF1,LF2
                     DO 105 LC = 1,CHNMAX
                        EDCORE(JPTR+1) = 1.0
                        JPTR = JPTR + NUMVAL
 105                    CONTINUE
 110                 CONTINUE
                  END IF
C                                       REAL/IMAG
               DO 125 IPRM = 2,3
                  ICL = ICL + 1
                  CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *               VALUE, CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  IF (IERR.LT.0) GO TO 185
                  IF (IFNOW.GE.0) THEN
                     JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
                     DO 120 LF = LF1,LF2
                        KF = (LF - 1) * CHNMAX + 1
                        DO 115 LC = 1,CHNMAX
                           EDCORE(JPTR+IPRM) = VALUE(KF)
                           JPTR = JPTR + NUMVAL
                           KF = KF + 1
 115                       CONTINUE
 120                    CONTINUE
                     END IF
 125              CONTINUE
C                                       tone frequencies
               ICL = ICL + 1
               CALL TABDGT (TBEDIT, IROW, COLNUM(ICL), TYPE, DIM,
     *            DVALUE, CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (IERR.LT.0) GO TO 185
               IF (IFNOW.GE.0) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
                  DO 130 LF = LF1,LF2
                     KF = (LF - 1) * CHNMAX + 1
                     DO 129 LC = 1,CHNMAX
                        DTEMP = DVALUE(KF)
                        EDCORE(JPTR+5) = RTEMP(1)
                        EDCORE(JPTR+6) = RTEMP(2)
                        JPTR = JPTR + NUMVAL
                        KF = KF + 1
 129                    CONTINUE
 130                 CONTINUE
                  END IF
C                                       real/imag -> amp/phase
               IF (IFNOW.GE.0) THEN
                  JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
                  DO 140 LF = LF1,LF2
                     DO 135 LC = 1,CHNMAX
                        W = EDCORE(JPTR+1)
                        DR = EDCORE(JPTR+2)
                        DI = EDCORE(JPTR+3)
                        IF ((DR.EQ.FBLANK) .OR. (DI.EQ.FBLANK) .OR.
     *                     ((DR.EQ.0.0) .AND. (DI.EQ.0.0))) THEN
                           EDCORE(JPTR+1) = -ABS(W)
                        ELSE
                           EDCORE(JPTR+2) = SQRT (DR*DR + DI*DI)
                           EDCORE(JPTR+3) = ATAN2 (DI, DR) /DG2RAD
                           END IF
                        JPTR = JPTR + NUMVAL
 135                    CONTINUE
 140                 CONTINUE
                  END IF
 150           CONTINUE
 185        LPTR = LPTR + DIMREC
 190        CONTINUE
         WASFLG = -1
         END IF
C                                       redo flags
      IF (IFNOW.GT.0) THEN
         CALL COPY (5, GRSEL(4), GRNONE)
         CALL FILL (5, 0, GRSEL(4))
         CALL BPFCDO (IERR)
         CALL COPY (5, GRNONE, GRSEL(4))
         END IF
      GO TO 999
C                                       write error
 990  WRITE (MSGTXT,1990) IREC, IROW, COLLAB(ICL)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('Found',I4,' times with',I7,' spectra total')
 1990 FORMAT ('PCGTPC: ERROR READING REC',I5,' ROW',I6,' COL ',A)
 4000 FORMAT ('Allocate ',I12,' words at',I18)
      END
      SUBROUTINE PCALIB
C-----------------------------------------------------------------------
C   PCALIB calls the delay/phase fitting routines for each polarization
C   IF and time to find the corrected phase values
C-----------------------------------------------------------------------
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PPCV.INC'
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C
      LONGINT   LPTR, JPTR
      INTEGER   IERR, LF1, LF2, IREC, IPOL, LF, KF, LC, IRET
      REAL      PCREAL(2,MAXTON), PCIMAG(2,MAXTON), RTEMP(2), PCDELY,
     *   PCPHAS, AMP, PHASE, WT, ERDELY, ERPHAS, WEIGHT
      DOUBLE PRECISION PCFREQ(2,MAXTON), DTEMP
      EQUIVALENCE (RTEMP, DTEMP)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      LF1 = BIF
      LF2 = EIF
      LPTR = DPTR
      IRET = 0
      MSGTXT = 'Fitting pulse-cal spectra for delay and phase'
      CALL MSGWRT (2)
      DO 50 IREC = 1,MAXREC
         DO 40 IPOL = 1,POLMAX
            JPTR = LPTR + DIMPRM + (IPOL-1) * DIMDAT * DIMIF
            DO 30 LF = LF1,LF2
               KF = (LF - LF1) * CHNMAX * NUMVAL
C                                       pick up one spectrum
               DO 10 LC = 1,CHNMAX
                  RTEMP(1) = EDCORE(JPTR+5+KF)
                  RTEMP(2) = EDCORE(JPTR+6+KF)
                  PCFREQ(1,LC) = DTEMP
                  WT = EDCORE(JPTR+1+KF)
                  AMP = EDCORE(JPTR+2+KF)
                  PHASE = EDCORE(JPTR+3+KF) * DG2RAD
                  IF ((WT.LE.0.0) .OR. (AMP.EQ.FBLANK)) THEN
                     PCREAL(1,LC) = FBLANK
                     PCIMAG(1,LC) = FBLANK
                  ELSE
                     PCREAL(1,LC) = AMP * COS (PHASE)
                     PCIMAG(1,LC) = AMP * SIN (PHASE)
                     END IF
                  KF = KF + NUMVAL
 10               CONTINUE
C                                       fit it
               CALL PCFITR (1, 1, 1, CHNMAX, PCFREQ, PCREAL, PCIMAG,
     *            0.0, PCDELY, PCPHAS, ERDELY, ERPHAS, WEIGHT, IERR)
               IRET = IRET + IERR
C                                       store resulting phase
               KF = (LF - LF1) * CHNMAX * NUMVAL
               DO 20 LC = 1,CHNMAX
                  IF ((PCREAL(1,LC).EQ.FBLANK) .OR.
     *               (PCIMAG(1,LC).EQ.FBLANK)) THEN
                     EDCORE(JPTR+4+KF) = FBLANK
                  ELSE
                     PHASE = ATAN2 (PCIMAG(1,LC), PCREAL(1,LC))
                     EDCORE(JPTR+4+KF) = PHASE * RAD2DG
                     END IF
                  KF = KF + NUMVAL
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
         LPTR = LPTR + DIMREC
 50      CONTINUE
C                                       failures?
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1050) IRET
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('PCALIB fit failed on',I10,' IF spectra')
      END
      SUBROUTINE BPMXMN (IAN, TYPE, PIXR, IERR)
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
      LONGINT   LPTR, JPTR, VPTR
      INTEGER   I, J, LP, LF, LR1, LR2, LR
      REAL      V, PPIXR(2)
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      PIXR(1) = 1.E10
      PIXR(2) = -1.E10
      PPIXR(1) = 1.E10
      PPIXR(2) = -1.E10
      LP = POLNOW
      JPTR = DIMPRM + (LP-1) * DIMIF * DIMDAT
      J = TYPE
      LR1 = EDCORI(PPTR+TIMEC)
      LR2 = EDCORI(PPTR+TIMEC+1) - 1
      DO 30 LR = LR1,LR2
         LPTR = DPTR + (LR - 1) * DIMREC
         IF (IAN.EQ.EDCORI(LPTR+1)) THEN
            VPTR = JPTR + LPTR
            DO 20 LF = 1,CHNTOT
               IF (EDCORE(VPTR+1).GT.0.0) THEN
                  V = EDCORE(VPTR+J+1)
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
               VPTR = VPTR + NUMVAL
 20            CONTINUE
            END IF
 30      CONTINUE
C                                       phase - go to plus
      IF ((TYPE.EQ.2) .AND. (PPIXR(2)-PPIXR(1).LT.PIXR(2)-PIXR(1))) THEN
         LPTR = DPTR
         JPTR = DIMPRM + (LP-1) * DIMIF * DIMDAT
         DO 70 I = 1,MAXREC
            IF (IAN.EQ.EDCORI(LPTR+1)) THEN
               VPTR = JPTR + LPTR
               DO 60 LF = 1,CHNTOT
                  IF (EDCORE(VPTR+1).GT.0.0) THEN
                     V = EDCORE(VPTR+3)
                     IF (V.NE.FBLANK) THEN
                        IF (V.LT.0.0) V = V + 360.0
                        EDCORE(VPTR+3) = V
                        END IF
                     END IF
                  VPTR = VPTR + NUMVAL
 60               CONTINUE
               END IF
            LPTR = LPTR + DIMREC
 70         CONTINUE
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
      SUBROUTINE BPMXMT (IAN, TYPE, PIXR, IERR)
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
      LONGINT   LPTR, JPTR, VPTR
      INTEGER   I, LP, LF
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
      LP = POLNOW
      LPTR = DPTR
      JPTR = DIMPRM + (LP-1) * DIMIF * DIMDAT
      DO 30 I = 1,MAXREC
         IF (IAN.EQ.EDCORI(LPTR+1)) THEN
            VPTR = LPTR + JPTR
            DO 20 LF = 1,CHNTOT
               IF (EDCORE(VPTR+1).GT.0.0) THEN
                  V = EDCORE(VPTR+3)
                  IF (V.NE.FBLANK) THEN
                     IF (V.LT.PIXR(1)) THEN
                        V = V + 360.0
                        IF (V.LE.PIXR(2)) EDCORE(VPTR+3) = V
                     ELSE IF (V.GT.PIXR(2)) THEN
                        V = V - 360.0
                        IF (V.GE.PIXR(1)) EDCORE(VPTR+3) = V
                        END IF
                     END IF
                  END IF
               VPTR = VPTR + NUMVAL
 20            CONTINUE
            END IF
         LPTR = LPTR + DIMREC
 30      CONTINUE
      IERR = 0
C
 999  RETURN
      END
      LOGICAL FUNCTION BPOANT (ALL, IAN, LAN)
C-----------------------------------------------------------------------
C   determines if the current antenna is included
C   Inputs:
C      ALL      I   < 0 -> IAN and LAN must match
C                   = 0 -> all antennas/baselines match
C                   = n > 0 -> match if n = M or N in LAN = 256*M + N
C      IAN      I   Antenna/baseline to include
C      LAN      I   test antenna/baseline
C   Output:
C      BPOANT   L   True if data included
C-----------------------------------------------------------------------
      INTEGER   ALL, IAN, LAN
C
C-----------------------------------------------------------------------
C                                       all
      IF (ALL.EQ.0) THEN
         BPOANT = .TRUE.
C                                       one only
      ELSE IF (ALL.GT.0) THEN
         BPOANT = IAN.EQ.LAN
         END IF
C
 999  RETURN
      END
      SUBROUTINE BPPLOT (IERR)
C-----------------------------------------------------------------------
C   plots the data and optionally the full data at the top
C   This routine sets plot coordinates, channels, ... and calls BPPLT1
C   to plot each portion of the plot.
C   Outputs:
C      IERR     I       Error code: > 0 quit
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I, J, N, ITRIM, JERR, HCS, LC, LC0, LIF, LC1, LC2
      REAL      FRAC
      CHARACTER STRING*80, CCPREF*5
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Plotting:'
      CALL MSGWRT (2)
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
      IF (CHAN2.LT.CHAN1) THEN
         CHAN1 = 1
         CHAN2 = CHNTOT
         END IF
C                                       Do plots: top
C                                       Edit data: set flags
      CALL TVDOPR (TVNAME, 'HOLD', I, JERR)
C                                       comparison plot then main
      IF (DOCOMP) THEN
         CALL RCOPY (2, DPIXR(1,LTYPE2), APIXR2)
         CALL BPPLT1 (PLTAN(1), 2, EXPLOT, GRSEL(4), GRSEL(5),
     *      GRSEL(7), CHAN1, CHAN2, 2, APIXR2, IERR)
         IF (IERR.GT.0) GO TO 980
         CALL RCOPY (2, DPIXR(1,LTYPE), APIXR(1,1))
         CALL BPPLT1 (PLTAN(1), 1, XYPLOT(1,1), -GRSEL(4), -GRSEL(5),
     *      -GRSEL(7), CHAN1, CHAN2, 2, APIXR(1,1), IERR)
C                                       main plot
      ELSE
         CALL RCOPY (2, DPIXR(1,LTYPE), APIXR(1,1))
         CALL BPPLT1 (PLTAN(1), 1, XYPLOT(1,1), GRSEL(4), GRSEL(5),
     *      GRSEL(7), CHAN1, CHAN2, 2, APIXR(1,1), IERR)
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
            CALL BPPLT1 (PLTAN(I), 1, XYPLOT(1,I), -GRSEL(6),
     *         -GRSEL(5), -GRSEL(7), CHAN1, CHAN2, 3, APIXR(1,I), IERR)
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
      WRITE (STRING(I:),1000) POLNOW
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
         WRITE (STRING(I:),1000) POLNOW
         N = ITRIM (STRING)
         I = (XYPLOT(1,1) + XYPLOT(3,1) - CSIZE(1)*N) / 2
         J = XYPLOT(4,1) + HCS + 1
         CALL TVDCHR (TVNAME, I, J, 0, 0, GRSEL(4), STRING(:N), IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       IFs
      J = LTVWND(2) + LEDG + 2 * HCS + CSIZE(2)
      LC0 = (CHNMAX+1) / 2
      DO 40 LIF = BIF,EIF
         LC1 = 1 + (LIF-BIF) * CHNMAX
         LC2 = LC1 + CHNMAX - 1
         LC = LC1 + LC0 - 1
         IF ((LC.LT.CHAN1) .AND. (LC2.GE.CHAN1)) THEN
            LC = (CHAN1 + MIN (LC2, CHAN2)) / 2
         ELSE IF ((LC.GT.CHAN2) .AND. (LC1.LE.CHAN2)) THEN
            LC = (CHAN2 + MAX (CHAN1, LC1)) / 2
         ELSE IF ((LC1.GT.CHAN2) .OR. (LC2.LT.CHAN1)) THEN
            LC = 0
            END IF
         IF (LC.GT.0) THEN
            FRAC = FLOAT(LC-CHAN1) / FLOAT (CHAN2-CHAN1)
            I = FRAC * (XYPLOT(3,1) - XYPLOT(1,1) - 2*LEDG) +
     *         XYPLOT(1,1) + 0.5 + LEDG
            WRITE (STRING,1020) LIF
            CALL CHTRIM (STRING, 14, STRING, N)
            CALL TVDCHR (TVNAME, I, J, 0, 0, GRSEL(4), STRING(:N), IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
 40      CONTINUE
C                                       status strings
      PNDING = .FALSE.
      CALL BPPLST (GRSEL(6), IERR)
      IF (IERR.NE.0) GO TO 980
      CALL TVDOPR (TVNAME, 'HOFF', I, JERR)
      TIMEL = TIMEC
      GO TO 999
C
 980  MSGTXT = 'BPPLOT: ERROR PLOTTING THE TV SCREEN'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POLARIZATION',I2)
 1020 FORMAT (I2)
      END
      SUBROUTINE BPPLSS (GR, IERR)
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
      INTEGER   I, J, HCS, ITT(4)
      REAL      TT
      CHARACTER STRING*28
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
      HCS = (CSIZE(2) + 1) / 2
      I = LTVWND(1) + CSIZE(1)
      J = LTVWND(2) + LEDG + HCS
C                                       flag all ant
      IF (ALLANT.LE.0) THEN
         STRING = 'DO ALL ANTENNAS'
      ELSE
         STRING = 'DO ONE ANTENNA '
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
C                                       flag all times
      IF (ALLTIM) THEN
         STRING = 'DO ALL TIMES'
      ELSE
         STRING = 'DO ONE TIME'
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
C                                       current time
      TT = TIMES(EDIPTR+TIMEC)
      CALL TODHMS (TT, ITT)
      WRITE (STRING,1000) ITT
      CALL TVDCHR (TVNAME, I, J, 0, 0, GR, STRING(:16), IERR)
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
 980  MSGTXT = 'BPPLSS: ERROR PLOTTING CL/SN STATUS STRINGS'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TIME',I3,'/',2(I2.2,':'),I2.2)
      END
      SUBROUTINE BPPLST (GR, IERR)
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
      IF (DDTYPE.EQ.'BP') THEN
         CALL BPPLSS (GR, IERR)
      ELSE IF (DDTYPE.EQ.'PC') THEN
         CALL BPPLSS (GR, IERR)
      ELSE
         IERR = 2
         END IF
C
 999  RETURN
      END
      SUBROUTINE BPPLT1 (IAN, IPRM, CORN, GR1, GR2, GR3, IT1, IT2,
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
      LONGINT   LPTR, JPTR, VPTR
      INTEGER   IG1, IG2, IX(5), IY(5), I, ITRIM, LTY, LF, LP1, LP2,
     *   LR1, LR2, LR
      REAL      V, XS, YS, PSCALE(2), LLPIXR(2)
      CHARACTER STRING*8
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C-----------------------------------------------------------------------
      LTY = LTYPE
      IF (IPRM.EQ.2) LTY = LTYPE2
      IERR = 2
      IG1 = ABS (GR1)
      IG2 = ABS (GR2)
      IF (IG1.EQ.0) GO TO 999
      IF (POLNOW.EQ.0) THEN
         LP1 = 1
         LP2 = 2
      ELSE
         LP1 = POLNOW
         LP2 = POLNOW
         END IF
C                                       zero the graphics memories
      IF (GR1.GT.0) THEN
         CALL TVDOPR (TVNAME, 'GRCL', GR1, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IF (GR2.GT.0) THEN
         CALL TVDOPR (TVNAME, 'GRCL', GR2, IERR)
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
      WRITE (STRING,1000) IAN
      I = 6
      IF (IAN.LT.100) I = 7
      IF (IAN.LT.10) I = 8
      IX(1) = CORN(3) - (10-I) * CSIZE(1)
      IY(1) = CORN(4) - 2 * CSIZE(2)
      CALL TVDCHR (TVNAME, IX, IY, 0, 0, IG1, STRING(I:), IERR)
      IF (IERR.NE.0) GO TO 999
C                                       find max min
      IF (LPIXR(2).LE.LPIXR(1)) THEN
         CALL BPMXMN (IAN, LTY, LLPIXR, IERR)
         IF (IERR.NE.0) THEN
            I = ITRIM (DTYPE(LTY))
            WRITE (MSGTXT,1010) DTYPE(LTY)(:I), IAN
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
         CALL BPMXMT (IAN, LTY, LLPIXR, IERR)
         IERR = 0
         END IF
C                                       loop through data to plot
      XS = (CORN(3) - CORN(1) - 2.*LEDG) / REAL (CHAN2-CHAN1)
      YS = (CORN(4) - CORN(2) - 2.*LEDG) / (LPIXR(2) - LPIXR(1))
      LR1 = EDCORI(PPTR+TIMEC)
      LR2 = EDCORI(PPTR+TIMEC+1) - 1
      DO 100 LR = LR1,LR2
         LPTR = DPTR + (LR - 1) * DIMREC
         IF (IAN.EQ.EDCORI(LPTR+1)) THEN
            JPTR = DIMPRM + (POLNOW-1) * DIMIF * DIMDAT
            VPTR = LPTR + JPTR + (CHAN1-1) * NUMVAL
            DO 90 LF = CHAN1,CHAN2
               IF (EDCORE(VPTR+1).NE.0.0) THEN
                  V = EDCORE(VPTR+LTY+1)
                  IF (V.NE.FBLANK) THEN
                     IF (LTY.EQ.2) THEN
                        IF (V.GT.LPIXR(2)) V = V - 360.0
                        IF (V.LT.LPIXR(1)) V = V + 360.0
                        END IF
                     IF ((V.GE.LPIXR(1)) .AND.(V.LE.LPIXR(2))) THEN
                        IY(1) = (V - LPIXR(1)) * YS + CORN(2) + 1.5 +
     *                     LEDG
                        IY(2) = IY(1) - 2
                        IX(1) = (LF-CHAN1) * XS + CORN(1) + 0.5 + LEDG
                        IX(2) = IX(1)
                        IF (EDCORE(VPTR+1).GT.0.0) THEN
                           CALL TVDLIN (TVNAME, 1, IG1, 2, IX, IY, IERR)
                        ELSE IF (IG2.GT.0) THEN
                           CALL TVDLIN (TVNAME, 1, IG2, 2, IX, IY, IERR)
                           END IF
                        IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 999
                        END IF
                     END IF
                  END IF
               VPTR = VPTR + NUMVAL
 90            CONTINUE
            END IF
 100     CONTINUE
      IERR = 0
C                                       frequency ticks
      IF (DOTICS.GE.0) THEN
         IY(1) = CORN(2)
         IY(2) = IY(1) + LEDG * 2
         IY(3) = CORN(4)
         IY(4) = IY(3) - LEDG * 2
         DO 120 LF = CHNMAX,CHNTOT-1,CHNMAX
            IF ((LF.GE.CHAN1) .AND. (LF.LE.CHAN2)) THEN
               IX(1) = (LF - CHAN1) * XS + CORN(1) + 0.5 + LEDG
               IX(2) = IX(1)
               CALL TVDLIN (TVNAME, 1, IG1, 2, IX, IY, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL TVDLIN (TVNAME, 1, IG1, 2, IX, IY(3), IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
 120        CONTINUE
         END IF
C                                       Y axis labeling
      IF (DOTICS.GT.0) THEN
         IF (DPLSCL(LTY).LE.0.0) DPLSCL(LTY) = 1.0
         PSCALE(1) = LPIXR(1) / DPLSCL(LTY)
         PSCALE(2) = LPIXR(2) / DPLSCL(LTY)
         CALL BPTICS (DOTICS, IPRM, CORN, IG1, PSCALE, IERR)
         IERR = MAX (IERR, 0)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I8)
 1010 FORMAT ('NO VALID DATA OF TYPE ',A,' FOR ANTENNA',I6)
      END
      SUBROUTINE BPPLTP (CORN, GR, LF, LF1, LF2, V, LPIXR, IERR)
C-----------------------------------------------------------------------
C   Plots one point
C   Inputs:
C      CORN     I(4)    TV corners for this plot
C      GR       I       Graphics channel (< 0 => erase the point)
C      LF       I       channel index into data for the 1 point
C      LF1      I       first channel plotted
C      LF2      I       last channel plotted
C      V        R       value of point
C      LPIXR    R(2)    Pixrange used in plot
C   Output:
C      IERR     I       error code: 0 okay, -1 off plot
C-----------------------------------------------------------------------
      INTEGER   CORN(4), GR, LF, LF1, LF2, IERR
      REAL      V, LPIXR(2)
C
      INTEGER   IG1, IX(2), IY(2)
      REAL      XS, YS
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IG1 = ABS (GR)
      IF (IG1.EQ.0) GO TO 999
      IERR = -1
C                                       loop through data to plot
      XS = (CORN(3) - CORN(1) - 2.*LEDG) / REAL (LF2 - LF1)
      YS = (CORN(4) - CORN(2) - 2.*LEDG) / (LPIXR(2) - LPIXR(1))
      IF ((V.GE.LPIXR(1)) .AND. (V.LE.LPIXR(2)) .AND. (LF1.LE.LF) .AND.
     *   (LF2.GE.LF) .AND. (V.NE.FBLANK)) THEN
         IY(1) = (V - LPIXR(1)) * YS + CORN(2) + 1.5 + LEDG
         IY(2) = IY(1) - 2
         IX(1) = (LF - LF1) * XS + CORN(1) + 0.5 + LEDG
         IX(2) = IX(1)
         IF (GR.GT.0) THEN
            CALL TVDLIN (TVNAME, 1, IG1, 2, IX, IY, IERR)
         ELSE
            CALL TVDLIN (TVNAME, 3, IG1, 2, IX, IY, IERR)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE BPSLST (IERR)
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
      SUBROUTINE BPSTUB (SUBR, OPCODE, OBJECT)
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
      SUBROUTINE BPTICS (DOTICS, IPRM, CORN, GR, PIXR, IERR)
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
      RPVAL(1,LOCNUM) = CHAN1
      AXINC(1,LOCNUM) = (CHAN2 - CHAN1) / (CORN(3) - CORN(1) - 7.)
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
      CALL BPTINC (BLC, TRC, NTMAX, DEGC, DEG, INOI, TICX, ITRY, IERR)
      IF (IERR.LT.0) GO TO 999
      IF (IERR.NE.0) GO TO 980
      DCX = -0.5 * CSIZE(1)
      DCY = -0.5 * CSIZE(2)
      IX(1) = CORN(1)
      IX(2) = CORN(1) + LEDG * 2
      IX(3) = CORN(3) - LEDG * 2
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
 980  MSGTXT = 'BPTICS: ERROR DRAWING Y-AXIS TICK MARKS'
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1270 FORMAT (F12.3)
 1271 FORMAT (F12.2)
 1272 FORMAT (F12.1)
      END
      SUBROUTINE BPTINC (BLC, TRC, NTMAX, DEGC, DEG, INOI, TICX, ITRY,
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
 1130 FORMAT ('BPTINC: TIC MARK ALGORITHM FAILED! CONTINUING.')
 1970 FORMAT ('BPTINC: POSITION ROUTINES FAIL ON THIS IMAGE')
      END
      SUBROUTINE BPTIMI (OT, IT)
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
      SUBROUTINE BPTIMX (IT, OT)
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
      INTEGER   I
      REAL      TEPS
      INCLUDE 'EDIUTIL.INC'
      INCLUDE 'EDIUTAP.INC'
C                                       0.095 sec
      DATA TEPS /1.1E-6/
C-----------------------------------------------------------------------
C                                       lower limit
      I = MAX (1, MIN (TIMEM, IT(1)))
      OT(1) = TIMES(EDIPTR+I) - EDCORE(TIPTR+I)/2.0 - TEPS
C                                       upper limit
      I = MAX (1, MIN (TIMEM, IT(2)))
      OT(2) = TIMES(EDIPTR+I) + EDCORE(TIPTR+I)/2.0 + TEPS
C
 999  RETURN
      END
      SUBROUTINE BPSORL (IT, NS, SL)
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
         J = EDCORI(J+SUPTR)
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
      SUBROUTINE BPTWIN (OPCODE, TYPE, IERR)
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
         IX(1) = (CHAN1 - 1.0) / (CHNTOT - 1.0) * (XYPLOT(3,1) -
     *      XYPLOT(1,1) - 2*LEDG) + 0.5 + XYPLOT(1,1) + LEDG
         IX(2) = IX(1)
         CALL TVDLIN (TVNAME, PT, GRSEL(3), 2, IX, IY, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       right
      IF ((TYPE.EQ.0) .OR. (TYPE.EQ.2)) THEN
         IX(1) = (CHAN2 - 1.0) / (CHNTOT - 1.0) * (XYPLOT(3,1) -
     *      XYPLOT(1,1) - 2*LEDG) + 0.5 + XYPLOT(1,1) + LEDG
         IX(2) = IX(1)
         CALL TVDLIN (TVNAME, PT, GRSEL(3), 2, IX, IY, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      GO TO 999
C
 980  MSGTXT = 'BPTWIN: ERROR PLOTING TOP PLOT''S WINDOW'
      CALL MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE BPWINC (XMIN, YMIN, IERR)
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
