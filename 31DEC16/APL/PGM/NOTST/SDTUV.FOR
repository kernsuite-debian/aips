LOCAL INCLUDE 'SDTUV.INC'
C                                       Local include for SDTUV
      INCLUDE 'INCS:ZPBUFSZ.INC'
C                                       Program common variables
      INTEGER   DISKI, SEQIN, INVER, SEQOUT, DISKO, NUMHIS, JBUFSZ,
     *   NANT, CATOLD(256), OLDCNO, NEWCNO, NUMVER, ILOCSC, ILOCSM,
     *   VISCNT, FRQSEL
      HOLLERITH XNAMEI(3), XCLASI(2), XXEXT, XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLASIN*6, XEXTIN*2, NAMOUT*12, CLAOUT*6,
     *   HISCRD(10)*64, ANTNAM*8, TABNAM*56, TELESC*8, INSTRU*8,
     *   OBSERV*8, TABSRC*8, REFDAT*8
      REAL      XSIN, XDISI, XVERIN, XSOUT, XDISO, APARM(10), BPARM(10),
     *   ARRAY2(20,2), TBUFF(UVBFSS), BUFFER(UVBFSS), XIAT, XUT1
      DOUBLE PRECISION ANTLOC(3,2), GST0
      INTEGER   HUGE, MAXRCX, LTBUFF(UVBFSS)
      LOGICAL   AVGSUB
      EQUIVALENCE (LTBUFF, TBUFF)
C                                       HUGE = size of MUTHA
      PARAMETER (HUGE = 256000)
C                                       MAXRCX = max. no receivers
      PARAMETER (MAXRCX = 64)
C                                       Table common variables
      LOGICAL   DOTIME, DOFREQ
      INTEGER   NUMROW, NUMCOL, NUMRCX, KPARM, KLENG, NUMST, NUMBM,
     *   NUMFQ, NUMIF, LCOR0, KLOCS, KLOCB, KLOCF, KLOCIF, KINCS,
     *   KINCB, KINCF, KINCIF, KLOCR, KLOCD, SCAN, CURREC, CURBEM
      REAL      RCXCAL(MAXRCX), RCXAVG(MAXRCX),
     *   LRCVR(MAXRCX), MRCVR(MAXRCX), EPOCH, TSTART, TINCR,
     *   BEMSIZ, BDF, FL, PTCON3, EXPT1, EXPT2, EXPT1N, EXPT2N,
     *   RABEM(MAXRCX), DECBEM(MAXRCX), GAIBEM(MAXRCX),
     *   LFMAX, LFMIN, MUTHA(HUGE)
      DOUBLE PRECISION RATAB, DECTAB, JDREF, FRQTAB, FRQINC
C                                       Local include for SDTUV
C                                       Program commons
      COMMON /ANTS/ ANTLOC, GST0, XIAT, XUT1, NANT
      COMMON /BUFRS/ BUFFER, TBUFF, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLASI, XSIN, XDISI, XXEXT, XVERIN,
     *   XNAMOU, XCLAOU, XSOUT, XDISO,
     *   APARM, BPARM, ARRAY2
      COMMON /SDTINF/ VISCNT, SEQIN, DISKI, INVER, SEQOUT, DISKO,
     *   OLDCNO, NEWCNO, CATOLD, NUMVER, ILOCSC, ILOCSM, NUMHIS,
     *   FRQSEL, AVGSUB
C                                       Table data common
      COMMON /TABDAT/ RATAB, DECTAB, JDREF, FRQTAB, FRQINC,
     *   RCXCAL, RCXAVG, LRCVR, MRCVR, EPOCH,
     *   TSTART, TINCR, BEMSIZ, BDF, FL, PTCON3, EXPT1, EXPT2,
     *   EXPT1N, EXPT2N, RABEM, DECBEM, GAIBEM, LFMAX, LFMIN, MUTHA,
     *   NUMROW, NUMCOL, NUMRCX, KPARM, KLENG, NUMST, NUMBM,
     *   NUMFQ, NUMIF, LCOR0, KLOCS, KLOCB, KLOCF, KLOCIF, KINCS,
     *   KINCB, KINCF, KINCIF, KLOCR, KLOCD, SCAN, CURREC, CURBEM,
     *   DOTIME, DOFREQ
C                                       Character data
       COMMON /CHRCOM/ NAMEIN, CLASIN, XEXTIN, NAMOUT, CLAOUT,
     *   HISCRD, ANTNAM, TABNAM, TELESC, INSTRU,
     *   OBSERV, TABSRC, REFDAT
LOCAL END
      PROGRAM SDTUV
C-----------------------------------------------------------------------
C! Converts single dish table file to uv like data.
C# Sdish EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2009, 2015
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
C   SDTUV convert single dish table files to uv like data.
C   Inputs:
C   INNAME.....Input SD table file name (name).
C   INCLASS....Input SD table file name (class).
C   INSEQ......Input SD table file name (seq. #).
C   INDISK.....Disk drive # of input table file.
C   INEXT......Table extension type. '  '=>'SD'
C   INVER......Table extension version. 0=> convert all.
C   OUTNAME....Output UV file name (name).    Standard behavior
C              with default 'UV DATA FILE'.
C   OUTCLASS...Output UV file name (class).   Standard defaults.
C   OUTSEQ.....Output UV file name (seq. #).  0 => highest unique.
C   OUTDISK....Disk drive # of output UV file. 0 => highest disk
C              with space for the file.
C   APARM......Control info:
C   BPARM......User specified array.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'SDTUV '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file.
      CALL SDTUVN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Convert table.
      CALL SDTGET (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL SDTHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE SDTUVN (PRGN, JERR)
C-----------------------------------------------------------------------
C   SDTUVN gets input parameters for SDTUV and creates an output file
C   if necessary.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                4 => error creating output file.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER CHSCAN*4, CHSAMP*4, ITYPE*2, STAT*4, PRGN*6, INEXT*2
      HOLLERITH CATOH(256)
      INTEGER  JERR, IERR
      INTEGER   NPARM, IROUND
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATOLD, CATOH)
      DATA CHSCAN, CHSAMP /'SCAN','SAMP'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
      VISCNT = 0
C                                       Initialization for SDTRED
      JDREF = 0.0D0
      TABNAM = '          '
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 76
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKI = IROUND (XDISI)
      INVER = IROUND (XVERIN)
      SEQOUT = IROUND (XSOUT)
      DISKO = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (2, 1, XXEXT, XEXTIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      IF (XEXTIN.EQ.'  ') XEXTIN = 'SD'
C                                       Lateral defocus correction
      BPARM(2) = BPARM(2) / 57.2957795
      IF (ABS (BPARM(2)).GT.1.0E-10)
     *   BPARM(2) = 0.693147181 / (BPARM(2) * BPARM(2))
C                                       See if baseline average
C                                       subtraction is requested.
      AVGSUB = ABS (APARM(5)) .GT. 1.0E-30

      OLDCNO = 1
      ITYPE = '  '
      CALL CATDIR ('SRCH', DISKI, OLDCNO, NAMEIN, CLASIN, SEQIN, ITYPE,
     *   NLUSER, STAT, BUFFER, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1030) JERR, NAMEIN, CLASIN, SEQIN, DISKI,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read old CATBLK - mark READ
      CALL CATIO ('READ', DISKI, OLDCNO, CATOLD, 'READ', BUFFER, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1040) JERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKI
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      FRQSEL = 1
C                                       Determine number of tables.
      IF (INVER.GT.0) THEN
         NUMVER = 1
      ELSE
         INVER = 1
         INEXT = XEXTIN
         CALL FNDEXT (INEXT, CATOLD, NUMVER)
         END IF
C                                       Read first table
      CALL SDTRED (INVER, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Create new header.
      CALL SDTHED (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get uv header info and
C                                       verify header structure.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Output SCAN pointer
      CALL AXEFND (4, CHSCAN, CATBLK(KIPCN), CATH(KHPTP), ILOCSC, IERR)
      IF (IERR.NE.0) ILOCSC = -1
C                                       Output SAMPLE pointer
      CALL AXEFND (4, CHSAMP, CATBLK(KIPCN), CATH(KHPTP), ILOCSM, IERR)
      IF (IERR.NE.0) ILOCSM = -1
C                                       Put new values in CATBLK.
C                                       Get naming defaults
      CALL MAKOUT (NAMEIN, '      ', 0, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
C                                       Image type ='UV'
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      NEWCNO = 1
      CALL UVCREA (DISKO, NEWCNO, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1050) IERR
         JERR = 4
         GO TO 990
 70   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
C                                       Get SEQ. no. used.
      SEQOUT = CATBLK(KIIMS)
      JERR = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDTUVN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE SDTGET (IRET)
C-----------------------------------------------------------------------
C   SDTGET loops over tables processes them and converts to uv like
C   data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER   OFILE*48
      INTEGER   IRET, IPTRO, LUNO, LENBU, NIOUT, INDO, LRECO, KBIND,
     *   TABVER, LIM1, LIM2, SUBA, IDSOU, IBB, NINT, JERR, VO, BO,
     *   NUMVIS, SVIS, EVIS
      REAL      BB, SC, SM, DUM, TIME, DT, DTNX, RADAT(64), DECDAT(64),
     *   TIMNXT, TIMLST
      LOGICAL   T, F
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNO /16/
      DATA VO, BO, LENBU /0, 1, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Setup for CS table
      SUBA = 1
      IF (APARM(1).GT.1.0E-20) THEN
         DT = APARM(1) / (24.0 * 60.0)
         TIMLST = TSTART
         IF (DOTIME) TIMLST = TSTART + TINCR * (NUMROW - 1)
         NINT = ((TIMLST - TSTART) / DT) + 0.999
         IF (NINT.LT.1) NINT = 1
         DT = (TIMLST - TSTART) / NINT
         TIMLST = 0.0
         END IF
C                                        Open vis file for write
      CALL ZPHFIL ('UV', DISKO, NEWCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 20
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Init vis file for write
C                                       LRECO = length of output rec.
 20   LRECO = LREC
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, LENBU, JBUFSZ,
     *   BUFFER, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NUMVIS = 0
      LIM1 = INVER
      IF (INVER.LT.1) LIM1 = 1
      LIM2 = LIM1 + NUMVER - 1
C                                       Loop over tables
      DO 200 TABVER = LIM1,LIM2
C                                       Read table to common; first
C                                       already done.
         IF(TABVER.GT.LIM1) CALL SDTRED (TABVER, JERR)
         IF (JERR.NE.0) GO TO 200
C                                       Any filtering here
         CALL SDTZAP
         CALL SDTTAB
         CALL SDTSPL
         CALL SDTMWF
C                                       Fix RA problem
         CALL BADRA
C                                       Fix 140 ft. dec problem
         CALL BADEC
C
C
         SVIS = VISCNT + 1
C                                       CS table control
         TIMNXT = 1.0E20
         IF (APARM(1).GT.1.0E-20) TIMNXT = TSTART + 0.6 * TINCR
C                                       Loop - processing table
 100     CONTINUE
            NUMVIS = NUMVIS + 1
C                                      Get datum
            CALL SDTCVT (NUMVIS, BUFFER(IPTRO+ILOCU),
     *         BUFFER(IPTRO+ILOCV), BUFFER(IPTRO+ILOCT), BB, SC, SM,
     *         BUFFER(IPTRO+NRPARM), IRET)
C                                       Branch on his return
            IF (IRET) 190, 130, 120
C                                       Error (fatal)
 120           WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Got datum
 130        VISCNT = VISCNT + 1
C                                       Fill in beam etc.
            IF (ILOCB.GE.0) BUFFER(IPTRO+ILOCB) = BB
            IF (ILOCSC.GE.0) BUFFER(IPTRO+ILOCSC) = SC
            IF (ILOCSM.GE.0) BUFFER(IPTRO+ILOCSM) = SM
            IBB = BB - 255.5
            IF ((IBB.LT.1) .OR. (IBB.GT.64)) IBB = 64
            RADAT(IBB) = BUFFER(IPTRO+ILOCU)
            DECDAT(IBB) = BUFFER(IPTRO+ILOCV)
            TIME = BUFFER(IPTRO+ILOCT)
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
C                                       Write vis record.
            CALL UVDISK ('WRIT', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
C                                       Check for end.
            IF (NIOUT.LE.0) GO TO 190
               IF (IRET.EQ.0) GO TO 160
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
 160           IPTRO = KBIND
               NIOUT = 0
               IF (TIME.GT.TIMNXT) THEN
C                                       calibration (CS) table entry.
                  CALL SDTCS (TIMNXT, SUBA, RADAT, DECDAT, NUMBM, NUMST,
     *               NUMIF, DISKO, NEWCNO, CATBLK, LTBUFF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  TIMLST = TIMNXT
                  TIMNXT = TIMNXT + DT
                  END IF
            GO TO 100
C                                       Read all of table
 190     IF (APARM(1).GT.1.0E-10) THEN
C                                       calibration (CS) table entry.
            IF (TIME.GT.TIMLST) THEN
               CALL SDTCS (TIME, SUBA, RADAT, DECDAT, NUMBM, NUMST,
     *            NUMIF, DISKO, NEWCNO, CATBLK, LTBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
            END IF
C                                       make Index (NX)
C                                       NX table
            EVIS = VISCNT
            IDSOU = 0
            DTNX = TIME - TSTART
            TIMLST = 0.5 * (TIME + TSTART)
            CALL SDTNX (SVIS, EVIS, TIMLST, DTNX, IDSOU, SUBA,
     *         DISKO, NEWCNO, CATBLK, FRQSEL, LTBUFF, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       End of table loop
 200        CONTINUE
C                                       Final call to SDTCVT.
         NUMVIS = -1
         CALL SDTCVT (NUMVIS, DUM, DUM, DUM, DUM, DUM, DUM, BUFFER,
     *      IRET)
         IF (IRET.LE.0) GO TO 205
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Finish write
 205  NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 210
         WRITE (MSGTXT,1150) IRET
         GO TO 990
C                                       Compress output file.
 210  NVIS = VISCNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close file
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDTGET: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('SDTGET: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1120 FORMAT ('SDTGET: SDTCVT ERROR',I3)
 1150 FORMAT ('SDTGET: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE SDTHIS
C-----------------------------------------------------------------------
C   SDTHIS creates and fills a history file.
C-----------------------------------------------------------------------
      CHARACTER   HILINE*72, ATIME*8, ADATE*12, LABEL*8, TELE*8, OBSR*8
      INTEGER   LUN1, LUN2, I, IERR, TIME(3), DATE(3)
      LOGICAL   T
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKI, DISKO, OLDCNO, NEWCNO,
     *   CATBLK, TBUFF, BUFFER, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (MSGTXT,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN2, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLASIN, SEQIN, DISKI, LUN2,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       INVER, INEXT
      WRITE (HILINE,2000) TSKNAM, INVER, XEXTIN
      CALL HIADD (LUN2, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Number of records
      WRITE (HILINE,2001) TSKNAM, CATBLK(KIGCN)
      CALL HIADD (LUN2, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Telescope, observer name.
      CALL H2CHR (8, 1, CATH(KHTEL), TELE)
      CALL H2CHR (8, 1, CATH(KHOBS), OBSR)
      WRITE (HILINE,2002) TSKNAM, TELE, OBSR
      CALL HIADD (LUN2, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       APARM
      WRITE (HILINE,2003) TSKNAM, APARM(1), APARM(2)
      CALL HIADD (LUN2, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Spline fitting
      IF (APARM(3).GT.0.01) THEN
         WRITE (HILINE,2004) TSKNAM, APARM(3), APARM(4)
         CALL HIADD (LUN2, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       Median window filter
      IF (APARM(5).GT.0.01) THEN
         WRITE (HILINE,2005) TSKNAM, APARM(5)
         CALL HIADD (LUN2, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       Interference detector
      IF (APARM(7).GT.1.0E-30) THEN
         WRITE (HILINE,2006) TSKNAM, APARM(7), APARM(8)
         CALL HIADD (LUN2, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       BPARM(1) (GB time problem)
      IF (BPARM(1).GT.0.01) THEN
         WRITE (HILINE,2010) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       Lateral focus gain correction
      IF (BPARM(2).GT.1.0E-10) THEN
         BPARM(2) = 57.2957795 * SQRT (0.693147181 / BPARM(2))
         WRITE (HILINE,2011) TSKNAM, BPARM(2)
         CALL HIADD (LUN2, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1015) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFFER, IERR)
C                                       Write ANtenna file.
      CALL ANTFIL
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDTHIS: ERROR',I3,' COPYING HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1015 FORMAT (A6,' /')
 2000 FORMAT (A6,'INVER =',I4,' INEXT = ''',A2,'''')
 2001 FORMAT (A6,' / Number of records copied=',I9)
 2002 FORMAT (A6,' / Telescope = ',A8,' Observer = ',A8)
 2003 FORMAT (A6,'APARM(1) =',F7.3,' APARM(2) =',F5.1,' /CS table,',
     *   ' scan flags')
 2004 FORMAT (A6,'APARM(3) =',F6.0,' APARM(4) =',1PE12.5,
     *   ' /Spline parameters')
 2005 FORMAT (A6,'APARM(5) =',F8.0,' / Median window filter width')
 2006 FORMAT (A6,'APARM(7) =',1PE12.5,',APARM(8) =',0PF8.0,
     *   ' /Interference detector')
 2010 FORMAT (A6,'BPARM(1) = 1  /Corrected GB time problem')
 2011 FORMAT (A6,'BPARM(2) =',1PE12.5,' / Beam size, lat. foc. corr.')
      END
      SUBROUTINE ANTFIL
C-----------------------------------------------------------------------
C   ANTFIL creates and fills the antenna file; one entry per beam is
C   made.
C-----------------------------------------------------------------------
      CHARACTER CHTEMP*4
      INTEGER   IERR, VER, LUN, I
      DOUBLE PRECISION JD, GMSTM, GASTM, RATE
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       One "antenna" per beam
      NANT = NUMBM
C                                       Make sure there is antenna info
      IF (NANT.LE.0) GO TO 999
C                                       Setup for AN table initization
         NUMORB = 0
         NOPCAL = 2
C                                       Position of the earth's pole
         POLRXY(1) = 0.0
         POLRXY(2) = 0.0
         UT1UTC = XUT1
         DATUTC = XIAT
C                                       Array name
         ANAME = ANTNAM
C                                       Array center (rel to center of
C                                       earth)
         ARRAYC(1) = 0.0D0
         ARRAYC(2) =  0.0D0
         ARRAYC(3) =  0.0D0
C                                       Get GST0 and Earth rotation rate
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         GSTIA0 = GST0
         IF (ABS (GST0).LT.1.0E-20) THEN
            CALL JULDAY (RDATE, JD)
            CALL GSTROT (JD, GMSTM, GASTM, RATE)
            GSTIA0 = GMSTM
            END IF
         DEGPDY = RATE
         SAFREQ = FREQ
         ANFQID = -1
         ANTNIF = NUMIF
         VER = 1
         TIMSYS = 'IAT'
C                                       Create/init file
         CALL ANTINI ('WRIT', BUFFER, DISKO, NEWCNO, VER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
         ANNAME = '        '
         STAXOF = 0.0
         STAXYZ(1) = 0.0D0
         STAXYZ(2) = 0.0D0
         STAXYZ(3) = 0.0D0
         ORBPRM(1) = 0.0D0
         NOSTA = 0
         MNTSTA = 0
         POLAA = 0.0
         POLAB = 0.0
         CALL RFILL (3, 0.0, POLCA)
         CALL RFILL (3, 0.0, POLCB)
         POLTYA = 'R'
         POLTYB = 'L'
         DIAMAN = 0.0
         CALL RFILL (ANTNIF, 0.0, FWHMAN)
C                                       AN records
         DO 20 I = 1,NANT
            STAXYZ(1) = ANTLOC(1,1)
            STAXYZ(2) = ANTLOC(2,1)
            STAXYZ(3) = ANTLOC(3,1)
            NOSTA = I
            WRITE (CHTEMP,1000) I
            IF (I.EQ.1) THEN
               ANNAME = ANTNAM
            ELSE
               ANNAME = 'BEAM' // CHTEMP
               END IF
            IANRNO = I
            CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV,
     *         ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *         DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB,
     *         POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
 20         CONTINUE
C                                       Fill in header and close
         CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4)
 1020 FORMAT ('ERROR ',I3,' OCCURED WRITING ANTENNA FILE')
      END
      SUBROUTINE SDTHED (IRET)
C-----------------------------------------------------------------------
C   SDTHED is a routine in which the catalog header is constructed.
C    Input:
C     CATBLK(256)    I     Output catalog header, also CATR, CATD
C                          The OUTNAME, OUTCLASS, OUTSEQ are entered
C                          elsewhere.
C    Output:
C     CATBLK(256)    I     Modified output catalog header.
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER RTYPES(7)*8, TYPES(7)*8, UNITS*8, TELE*8, OBSR*8,
     *   INSTR*8, OBSDAT*8
      INTEGER   IRET,
     *   I, NAXIS, NRAN, NCHAN, NPOLN, NIF, NDIM(7), INDEX, XCOUNT
      REAL      CRPIX(7),  CRINC(7), BANDW
      DOUBLE PRECISION CRVAL(7)
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C                                       Random parameters.
C                                         No. random parameters.
      DATA NRAN /6/
C                                         Rand. parm. names.
C                                         This order is important.
      DATA RTYPES /'RA      ','DEC     ','TIME1   ',
     *   'BEAM    ', 'SCAN    ','SAMPLE  ','        '/
C                                       Uniform axes.
C                                         No. axes.
      DATA NAXIS /5/
C                                         Axes names.
      DATA TYPES /'COMPLEX ','STOKES  ','FREQ    ',
     *   'RA      ','DEC     ', 'IF      ', '        '/
C                                         Axis dimensions
      DATA NDIM /3,1,1,1,1,0,0/
C                                         Reference values
      DATA CRVAL /1.0D0, -1.0D0, 5*0.0D0/
C                                         Reference pixel.
      DATA CRPIX /7*1.0/
C                                         Coordinate increment.
      DATA CRINC /1.0, -1.0, 1.0, 1.0, 1.0, 2*1.0/
C                                       Units
      DATA UNITS /'KELVIN  '/
C-----------------------------------------------------------------------
      GST0 = 0.0D0
      XIAT = 0.0
      XUT1 = 0.0
C                                       Drop BEAM if only 1
      IF (NUMBM.LE.1) THEN
         NRAN = NRAN - 1
C                                       Collapse name array.
         DO 10 I = NRAN,7
            RTYPES(I) = RTYPES(I+1)
 10         CONTINUE
         END IF
C                                       SCAN, SAMPLE parameters.
      IF (APARM(2).GE.0.0) THEN
C                                       No SAMPLE for DOFREQ=true
         IF (DOFREQ) NRAN = NRAN - 1
      ELSE
C                                       Drop SCAN, SAMPLE
         NRAN = NRAN - 2
         END IF
C                                       Zero fill CATBLK
      CALL FILL (256, 0, CATBLK)
C                                       Random axis names
      DO 20 I = 1,KIPTPN
         INDEX = KHPTP + (I-1) * 2
         IF (I.LE.NRAN) THEN
            CALL CHR2H (8, RTYPES(I), 1, CATH(INDEX))
         ELSE
            CALL CHR2H (8, '        ', 1, CATH(INDEX))
            END IF
 20      CONTINUE
C                                       Uniform axes
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = NDIM(I)
C                                       Init. increment.
         CATR(KRCIC+I-1) = CRINC(I)
C                                       Init. rotation.
         CATR(KRCRT+I-1) = 0.0
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = CRPIX(I)
C                                       Init. ref value.
         CATD(KDCRV+I-1) = CRVAL(I)
C                                       Fill axis type from
C                                       TYPES
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, TYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C                                       Source name
      SOURCE = TABSRC
C                                       Observation date.
      OBSDAT = REFDAT
C                                       Telescope.
      TELE = TELESC
C                                       Receiver
      INSTR = INSTRU
C                                       Observer's name.
      OBSR = OBSERV
C                                       Set number of axes.
C                                       IF axis?
      IF (KLOCIF.GE.0) NAXIS = NAXIS + 1
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRAN
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = EPOCH
C                                       Convolving beam
      CATR(KRBMJ) = BEMSIZ
      CATR(KRBMN) = BEMSIZ
      CATR(KRBPA) = 0.0
      CATBLK(KINIT) = 0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Shift
      CATR(KRXSH) = 0.0
      CATR(KRYSH) = 0.0
C                                       "Old" (observed) position.
      CATD(KDORA) = 0.0D0
      CATD(KDODE) = 0.0D0
C                                       Rest Frequency
      CATD(KDRST) = 0.0D0
C                                       Alternate ref. value & pixel
      CATD(KDARV) = 0.0D0
      CATR(KRARP) = 0.0
      CATBLK(KIALT) = 0
C                                       Sort order ('TB'=>time-beam)
      CALL CHR2H (2, 'TB', 1, CATH(KITYP))
C                                       No magic value blanking.
      CATR(KRBLK) = 0.0
C                                       Units
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C                                       Number of records (add 10%)
      XCOUNT = (NUMBM * NUMROW * NUMVER) * 1.10
C                                       RA = Right ascension (1950)
C                                          in degrees.
      RA = 0.0D0
      IF (KLOCR.LT.0) RA = RATAB
C                                       DEC = Declination in degrees.
      DEC = 0.0D0
      IF (KLOCD.LT.0) DEC = DECTAB
C                                       FREQ = frequency of obs in Hz.
      FREQ = FRQTAB
C                                       BANDW = bandwidth or channel
C                                           separation.
      BANDW = FRQINC
C                                       NCHAN = Number of freq chan.
      NCHAN = NUMFQ
C                                       NPOLN = number of polarization
C                                            correlators.
      NPOLN = NUMST
C                                       NIF = number of IFs.
      NIF = NUMIF
C                                       Insert values in header.
C                                       Number of vis.
      CATBLK(KIGCN) = XCOUNT
C                                       Position.
      CATD(KDCRV+3) = RA
      CATD(KDCRV+4) = DEC
C                                       Frequency
      CATD(KDCRV+2) = FREQ
C                                       Bandwidth.
      CATR(KRCIC+2) = BANDW
C                                       Number of frequencies.
      CATBLK(KINAX+2) = NCHAN
C                                       Number of polarizations.
      CATBLK(KINAX+1) = NPOLN
C                                       Number of IFs.
      CATBLK(KINAX+5) = NIF
C                                       Observing date.
      CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
C                                       Object.
      CALL CHR2H (8, SOURCE, 1, CATH(KHOBJ))
C                                       Telescope.
      CALL CHR2H (8, TELE, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, INSTR, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, OBSR, 1, CATH(KHOBS))
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE SDTCVT (NUMVIS, RR, DD, T, BB, SC, SM, VIS, IRET)
C-----------------------------------------------------------------------
C  This routine picks values out of common and returns then one at a
C  time.
C    Positions are corrected and sky brightnesses are corrected by
C    RCXAVG.  Position correction is done by computing a tangent
C    projected offset of each feed and using this to correct the
C    position.  The tangent offset is computed from several telescope
C    pointing parameters and the scheme given may only work for the
C    Green Bank 300 ft.
C  Inputs:
C  NUMVIS     I    Visibility number, -1 => final call, no data
C                  passed but allows any operations to be completed.
C  Inputs from COMMON
C  NRPARM     I    # random parameters.
C  NCOR       I    # correlators
C  CATBLK(256)I    Catalog header record.
C  BPARM(10)  R    Control info
C
C  Output:
C  RR         R    RA of observation (deg)
C  DD         R    Declination of observation (deg)
C  T          R    Time in days since the midnight of ref. date.
C  BB         R    Beam number + 256
C  SC         R    Scan number
C  SM         R    Sample number
C  VIS(3,*)   R    Visibilities.  The first dimension is the COMPLEX
C                  axis in the order Real part, Imaginary part, weight.
C                  The "imaginary" part is the baseline that was
C                  subtracted.
C  IRET       I    Return code  -1 => End of data.
C                                0 => OK
C                               >0 => error, terminate.
C
C  Output in COMMON
C  NUMHIS     I    # history entries (max. 10)
C  HISCRD(16,NUMHIS) R   History records
C  CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IRET, NUMVIS
      REAL      RR, DD, T, BB, SC, SM, VIS(3,*)
      INTEGER   LOOPS, LOOPF, LOOPIF, IPOINT, IRCX
      LOGICAL   ALLBAD
      INTEGER   INDXRC, INDEX, JNDEX
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
 10   ALLBAD = .TRUE.
C                                       Check if done
      IF (CURREC.GT.NUMROW) THEN
         IRET = -1
         GO TO 999
         END IF
C                                       Work to do
      IRET = 0
      IF (NUMVIS.LT.0) GO TO 900
      IF (.NOT. (DOTIME.OR.DOTIME)) THEN
C                                       Don't know what to do
         IRET = 1
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
C                                       Init. positions
      IF (CURBEM.EQ.1) CALL SDTPOS
C                                       Time ordered
      INDXRC = (CURREC-1) * KLENG + 1
C                                       Frequency ordered
      IF (DOFREQ) INDXRC = 1
C                                       Random parameters
C                                       RA
      RR = RABEM(CURBEM)
C                                       Dec.
      DD = DECBEM(CURBEM)
C                                       Time
      T = TSTART + TINCR * (CURREC-1)
C                                       Beam
      BB = CURBEM + 256.0
C                                       Scan
      SC = SCAN
C                                       Sample
      SM = CURREC
C                                       Data
      IPOINT = 1
      INDXRC = INDXRC + KPARM + (CURBEM-1) * KINCB
      DO 100 LOOPIF = 1,NUMIF
         DO 100 LOOPF = 1,NUMFQ
            JNDEX = INDXRC + (LOOPF-1) * KINCF + (LOOPIF-1) * KINCIF
            DO 100 LOOPS = 1,NUMST
               IRCX = (CURBEM-1) + (KLOCS +
     *            (LOOPS-ABS (LCOR0)) * KINCS)/3 + 1
               INDEX = JNDEX + (LOOPS-ABS (LCOR0)) * KINCS + KLOCS
               VIS(1,IPOINT) = (MUTHA(INDEX) - RCXAVG(IRCX)) *
     *            GAIBEM(CURBEM)
               VIS(2,IPOINT) = (MUTHA(INDEX+1) + RCXAVG(IRCX)) *
     *            GAIBEM(CURBEM)
               VIS(3,IPOINT) = MUTHA(INDEX+2)
               ALLBAD = ALLBAD .AND. (MUTHA(INDEX+2).LE.1.0E-30)
               IPOINT = IPOINT + 1
 100           CONTINUE
C                                       Flag for excessive lateral
C                                       defocus correction.
      ALLBAD = ALLBAD .OR. (GAIBEM(CURBEM).GT.10.0)
C                                       Update pointers
      CURBEM = CURBEM + 1
      IF (CURBEM.GT.NUMBM) THEN
         CURBEM = 1
         CURREC = CURREC + 1
         END IF
C                                       If record all bad, get another
      IF (ALLBAD) GO TO 10
      GO TO 999
C                                       Any cleanup
 900  CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DATA NEITHER TIME NOR FREQUENCY - I CANNOT COPE')
      END
      SUBROUTINE SDTPOS
C-----------------------------------------------------------------------
C   Routine to compute actual pointing positions and beam lateral
C   defocus corrections for all beams at a given time.
C   Currently may only work for NRAO 300 ft.
C    Input from common:
C     BPARM        R    If (2)>0 then this is 1 / 2* (beam size )**2
C     NUMROW       I    Number of rows in the table.
C     LRCVR(64)    R    Receiver RA offset (deg)
C     MRCVR(64)    R    Receiver Declination offset (deg)
C     BDF          R    Beam deflection factor
C     FL           R    Focal length (m)
C     EXPT1,EXPT2  R    Actual pointing constants
C     EXPT1N,EXPT2NR    Nominal pointing constants
C     LFMIN,LFMAX  R    min, max lateral focus (units of EXPT1,EXPT2)
C     KLENG        I    Length of table record as kept in common.
C     NUMBM        I    Number of beams.
C     KLOCR        I    Order number of RA, <0 => use RATAB
C     KLOCD        I    Order number of Dec, <0 => use DECTAB
C     RATAB        D    RA of table data (deg)
C     DECTAB       D    Declination of table data.
C     CURBEM       I    Current beam pointer (used by SDTCVT)
C     MUTHA(huge)  R    Data array, See above for a description of the
C                       data ordering.
C    Output to common:
C     RABEM(*)     R    RA(Epoch) of each beam in degrees
C     DECBEM(*)    R    Declination (Epoch) of each beam in degrees.
C     GAIBEM(*)    R    Factor to correct for lateral defocus.
C                       If > 10.0 then flag data.
C-----------------------------------------------------------------------
      INTEGER   IRCX, INDXRC
      REAL      RR, DD, LCOR, LBOX, MCOR, MBOX, DD0R, CDD0R, SDD0R,
     *   DELALF, DELDEC, LFACT, LFERR
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Get nominal positions, time
C                                       order
      INDXRC = (CURREC-1) * KLENG + 1
C                                       Frequency ordered
      IF (DOFREQ) INDXRC = 1
C                                       RA
      RR = RATAB
      IF (KLOCR.GE.0) RR = MUTHA(INDXRC+KLOCR)
C                                       Declination
      DD = DECTAB
      IF (KLOCD.GE.0) DD = MUTHA(INDXRC+KLOCD)
      DD0R = DD * 1.74533E-2
      CDD0R = COS (DD0R)
      SDD0R = SIN (DD0R)
C                                       Box offset
C                                       This may not be general
      LBOX = - PTCON3 * BDF / FL
      LFACT = EXPT1 + DD0R * EXPT2
      LFACT = MIN (LFACT, LFMAX)
      LFACT = MAX (LFACT, LFMIN)
C                                       Lateral focus error
      LFERR = (EXPT1N + DD0R * EXPT2N) - LFACT
      MBOX = LFERR * BDF / FL
C                                       Loop over receivers
      DO 100 IRCX = 1,NUMBM
         LCOR = LBOX + LRCVR(IRCX)
         MCOR = MBOX + MRCVR(IRCX)
C                                       Apply corrections: tan
C                                       projection offsets.
         DELALF = ATAN (LCOR / (CDD0R - MCOR * SDD0R))
         RABEM(IRCX) = RR + DELALF * 57.2957795
         DECBEM(IRCX) = ATAN (COS (DELALF) * ((MCOR * CDD0R + SDD0R) /
     *      (CDD0R - MCOR * SDD0R)))
         DELDEC = DD0R - DECBEM(IRCX)
         DELALF = DELALF * CDD0R
         DECBEM(IRCX) = DECBEM(IRCX) * 57.2957795
         GAIBEM(IRCX) = (DELALF*DELALF + DELDEC*DELDEC) * BPARM(2)
 100     CONTINUE
C                                       Lateral focus gain corrections
      IF (BPARM(2).LE.1.0E-10) THEN
C                                       No correction
         DO 120 IRCX = 1,NUMBM
            GAIBEM(IRCX) = 1.0
 120        CONTINUE
      ELSE
C                                       Avoid trouble
         DO 130 IRCX = 1,NUMBM
            IF (GAIBEM(IRCX).GT.3.0) GAIBEM(IRCX) = 3.0
 130        CONTINUE
C                                       Gain corrections
         DO 140 IRCX = 1,NUMBM
               GAIBEM(IRCX) = EXP (GAIBEM(IRCX))
 140        CONTINUE
      END IF
C
 999  RETURN
      END
      SUBROUTINE SDTRED (TABVER, IRET)
C-----------------------------------------------------------------------
C      Reads a single dish (SD) table and saves its partially digested
C   contents in common.  The tabulated data is kept in an array which
C   in generally of the form of AIPS random group UV data and
C   parameters which allow addressing data in the array are also passed
C   in common.
C***********************************************************************
C                       Notice
C      This routine needs lots of work to interprete the general case
C   Single dish table.  Currently it understands only about 300 ft.
C   7 feed continuum data.
C***********************************************************************
C      Data in common may have two "random" parameters which are
C   assumed to be RA and declination.  If all of the data from the table
C   is from the same celestial pointing then the RA and Dec are given
C   as RATAB and DECTAB.  KLOCR and KLOCD tell how to determine the RA
C   and Dec.
C      Data in common has been expanded to 3 words per sky brightness
C   measurment.  The first is the sky brightness, the second is and
C   baseline that has been removed from the sky brightness and the
C   third is a weight, nonpositive means invalid data.  This data may
C   be copied to AIPS random parameter data in this form.  The data is
C   assumed to consist of either a time sequence or a frequency
C   sequence; DOTIME and DOFREQ say which.
C      Currently up to 4 regular axes are described 1) Stokes parameter
C   (may be either RCP and/or LCP or true Stokes), 2) Beam (for
C   multibeam systems), 3) Frequency channel or 4) IF (for multiple
C   spectrum data).  The Sky brightness word of Stokes' ISTOK, beam
C   IBEAM, Frequency channel IFREQ, IF IIF and record IREC is given by:
C     (IREC-1) * KLENG + (ISTOK-ABS (LCOR0))*KINCS + (IBEAM-1)*KINCB +
C        (IFREQ-1)*KINCF + (IIF-1)*KINCIF
C
C    Inputs:
C     TABVER       I    Table version to read.
C    Outputs:
C     IRET         I    Return code, 0=> OK else failed.
C    Output to Antenna common (first call):
C     NANT         I    No. antennas
C     ANTLOC(3,1)  D    Antenna location (recognized antennas only)
C     ANTNAM       C*8  Antenna name
C    Output to Common:
C     TABNAM       C*56  Table name
C     NUMROW       I    Number of rows in the table.
C     NUMCOL       I    Number of columns in table.
C     NUMRCX       I    Number of receivers
C     RCXCAL(64)   R    Receiver TCAL/CALVOLTS these are applied.
C     RCXAVG(64)   R    Average receiver value (after RCXCAL applied)
C     LRCVR(64)    R    Receiver RA offset (deg)
C     MRCVR(64)    R    Receiver Declination offset (deg)
C     BDF          R    Beam deflection factor
C     FL           R    Focal length (m)
C     EXPT1,EXPT2  R    Actual pointing constants
C     EXPT1N,EXPT2NR    Nominal pointing constants
C     LFMIN,LFMAX  R    min, max lateral focus (units of EXPT1,EXPT2)
C     KPARM        I    Number of words of "random" data before data.
C     KLENG        I    Length of table record as kept in common.
C     NUMST        I    Number of Stokes' parameters.
C     NUMBM        I    Number of beams.
C     NUMFQ        I    Number of frequency channels.
C     NUMIF        I    Number of IFs present.
C     LCOR0        I    First Stokes' parameter:
C                       -1 => RCP, -2 => LCP,
C                       1 => I, 2 => Q, 3 => U, 4 => V.
C     KLOCS        I    Offset for first Stokes'
C     KLOCB        I    Order number of Beam number
C     KLOCF        I    Order number of Frequency channels.
C     KLOCIF       I    Order number of IF
C     KINCS        I    Increment (words) between Stokes'
C     KINCB        I    Increment (words) between beams
C     KINCF        I    Increment (words) between Frequency channels,
C                       for frequency ordered data this will be the
C                       increment between rows.
C     KINCIF       I    Increment (words) between IFs.
C     KLOCR        I    Order number of RA, <0 => use RATAB
C     KLOCD        I    Order number of Dec, <0 => use DECTAB
C     RATAB        D    RA of table data (deg)
C     DECTAB       D    Declination of table data.
C     EPOCH        R    Mean Epoch of RA and Declination
C     JDREF        D    Julian data of reference day.
C     REFDAT       C*8  Reference day as 'yyyymmdd'
C     TSTART       R    IAT time of first pixel (in days from ref.
C                       date.)
C     TINCR        R    Time increment (days)
C     DOTIME       L    If true data is a time sequence.
C     FRQTAB       D    Frequency (Hz) of first element.
C     FRQINC       D    Frequency increment or bandwidth
C     DOFREQ       L    If ture, data is a frequency sequence.
C     SCAN         I    Scan number
C     BEMSIZ       R    Beam size (deg)
C     TELESC       C*8  Telescope name
C     INSTRU       C*8  Receiver name
C     OBSERV       C*8  Observer's name
C     TABSRC       C*8  Source name
C     CURREC       I    Current record pointer (used by SDTCVT)
C     CURBEM       I    Current beam pointer (used by SDTCVT)
C
C     MUTHA(huge)  R    Data array, See above for a description of the
C                       data ordering.
C-----------------------------------------------------------------------
      CHARACTER XPOLR(64)*8, INEXT*2, KEYGEN(24)*8, KEYRCX(7)*8,
     *   KEYW(20)*8, KNOTAB(1)*16, KNTELE(14)*8, TBNTMP*56, CHTMP*2,
     *   XVELD*2
      INTEGER   TABVER, IRET
      HOLLERITH RECH(50)
      REAL      RECORD(50), XTEMPR(2), KNTELP(4,14)
      INTEGER   KEYPNT, LOCS(25), KEYTYP(25), NKEY, NREC, NCOL,
     *   DATP(2,128), LUN, ITBUFF(512), NGET, IRCX, IERR,
     *   TABTYP, NKTAB, ITABTY, IYR, IMN, IDAY, NKANT, IDANT,
     *   LOOP, NUMDAT, NRCX, NDATA, NWRDS(3), RECI(50), I,
     *   KBAD, NCPRCX(7), RECNO, INDEX
      DOUBLE PRECISION    RECD(25), XSCAN, XORI, XEXPT(4), XDATE, XTIME,
     *   XCYCL, XSAMP, XEPOC, XERA, XEDEC, XBMSZ, XOFFS, XVEL,
     *   XFREQ(64), XTCAL(64), XRHO(64), XTHETA(64), XCALV(64),
     *   XBW(64), XPTCN1, XPTCN2, XPTCN3, KNLOC(3,14),
     *   JD, XTEMPD
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECI, RECH, RECORD, RECD),   (TBUFF, ITBUFF)
      EQUIVALENCE (XTEMPR, XTEMPD)
C                                       General keywords
C                     1            2           3
      DATA KEYGEN /'SCAN    ', 'OBSERVER', 'TELESCOP',
C          4             5         6            7
     *   'OBJECT  ', 'FRONTEND', 'ORIENT  ', 'EXPT1   ',
C          8            9          10          11
     *   'EXPT2   ', 'EXPT3   ', 'EXPT4   ', 'UTDATE  ',
C          12          13          14          15
     *   'HOURUT  ', 'CYCLLEN ', 'SAMPRAT ', 'EPOCH   ',
C          16          17          18          19
     *   'EPOCRA  ', 'EPOCDEC ', 'BFWHM   ', 'OFFSCAN ',
C          20           21
     *   'VELOCTY ', 'VELDEF  ',
C          22           23         24
     *   'PTCON1  ', 'PTCON2  ', 'PTCON3  '/
C                                       Receiver keywords
C                     1           2           3
      DATA KEYRCX /'OBSFRE01', 'TCAL01  ', 'POLARI01',
C          4            5           6
     *   'RHO01   ', 'THETA01 ', 'CALVOL01',
C          7
     *   'BW01    '/
C                                       Pointers to KEYRCX
      DATA NCPRCX /7,5,7,4,6,7,3/
C                                       Recognized tables
      DATA NKTAB /1/
      DATA KNOTAB /'93M CONT        '/
C                                       Recognized antennas
      DATA NKANT /14/
      DATA KNTELE /'DUMMY   ', 'NRAO12M ', 'NRAO42M ',
     *   'NRAO93M ', 'MPI100M ', 'IRAM30M ', 'NRO45M  ',
     *   'PMO14M  ', 'OSO20M  ', 'MASS14M ', 'UTX5M   ',
     *   'UK-D15M ', 'IRAM15M ', 'PKS64M'/
C                                       Focal length, BDF, min, max.
C                                       lateral focus.
      DATA KNTELP /0.0,0.0,0.0,0.0,
C       NRAO12M              NRAO42M         NRAO93M
     *   0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 37.74,0.87,-0.279,0.292,
C       others (need)
     *   40*0.0/
C                                       Positions
      DATA KNLOC /0.0D0, 0.0D0, 0.0D0,
C                                       NRAO12m
     *   3*0.0D0,
C                                       NRAO42m
     *   882882.5,4924484.0,3944130.9,
C                                       NRAO93m
     *   881865.5,4925242.0,3943433.9,
C                                       MPI100m
     *   4033942.1,-486993.1,4900431.8,
C                                       IRAM30m need posn.
     *   3*0.0D0,
C                                       NRO45m
     *   -3871071.0,-3428034.0,3723788.0,
C                                       PMO14m need posn (Purple Mtn?)
     *   3*0.0D0,
C                                       OSO20m
     *   3370600.5,-711919.2,5349831.9,
C                                       MASS14m
     *   1430973.0,4495743.0,4278052.0,
C                                       UTX5m need posn.
     *   3*0.0D0,
C                                       UK-D15m need posn.
     *   3*0.0D0,
C                                       IRAM15m need posn.
     *   3*0.0D0,
C                                       Parkes 64 m
     *   -4554237.382,-2816745.713,-3454030.728/
      DATA LUN /28/
C-----------------------------------------------------------------------
C                                       Open table
      NKEY = 0
      NREC = 0
      NCOL = 0
      INEXT = XEXTIN
      CALL TABINI ('READ', INEXT, DISKI, OLDCNO, TABVER, CATOLD, LUN,
     *   NKEY, NREC, NCOL, DATP, ITBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
C                                       Table name; check with previous
      CALL H2CHR (56, 1, ITBUFF(101), TBNTMP)
      IF ((TABNAM(1:10).NE.'          ') .AND. (TABNAM.NE.TBNTMP)) THEN
         IRET = 7
         WRITE (MSGTXT,1001)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1002) TBNTMP
         CALL MSGWRT (8)
         WRITE (MSGTXT,1003)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1002) TABNAM
         GO TO 990
         END IF
      TABNAM = TBNTMP
C                                       Number of rows
      NUMROW = ITBUFF(5)
C                                       Number of columns
      NUMCOL = NCOL
C                                       Crunch keywords.
      NKEY = 24
      CALL TABKEY ('READ', KEYGEN, NKEY, ITBUFF, LOCS, RECI, KEYTYP,
     *   IERR)
      IF ((IERR.GT.0) .AND. (IERR.LE.20)) THEN
C                                       Error with TABKEY
         WRITE (MSGTXT,1100) IERR
         IRET = 2
         GO TO 990
         END IF
      NWRDS(1) = NWDPDP
      NWRDS(2) = 1
      NWRDS(3) = 2
C                                       Scan number
      KEYPNT = LOCS(1)
      I = KEYTYP(1)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XSCAN = XTEMPD
      IF (I.EQ.2) XSCAN = XTEMPR(1)
C                                       Observer
      KEYPNT = LOCS(2)
      OBSERV = '        '
      IF (KEYPNT.GT.0) THEN
         CALL H2CHR (8, 1, RECH(KEYPNT), OBSERV)
         KBAD = 2
         IF (KEYTYP(2).NE.3) GO TO 110
         END IF
C                                       Telescope
      KEYPNT = LOCS(3)
      TELESC = '        '
      IF (KEYPNT.GT.0) THEN
         CALL H2CHR (8, 1, RECH(KEYPNT), TELESC)
         KBAD = 3
         IF (KEYTYP(3).NE.3) GO TO 110
         END IF
C                                       Object
      KEYPNT = LOCS(4)
      TABSRC = '        '
      IF (KEYPNT.GT.0) THEN
         CALL H2CHR (8, 1, RECH(KEYPNT), TABSRC)
         KBAD = 4
         IF (KEYTYP(4).NE.3) GO TO 110
         END IF
C                                       Instrument
      KEYPNT = LOCS(5)
      INSTRU = '        '
      IF (KEYPNT.GT.0) THEN
         CALL H2CHR (8, 1, RECH(KEYPNT), INSTRU)
         KBAD = 5
         IF (KEYTYP(5).NE.3) GO TO 110
         END IF
C                                       Orientation
      KEYPNT = LOCS(6)
      I = KEYTYP(6)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XORI = XTEMPD
      IF (I.EQ.2) XORI = XTEMPR(1)
C                                       EXPTn
      KEYPNT = LOCS(7)
      I = KEYTYP(7)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XEXPT(1) = XTEMPD
      IF (I.EQ.2) XEXPT(1) = XTEMPR(1)
      KEYPNT = KEYPNT + NWRDS(I)
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XEXPT(2) = XTEMPD
      IF (I.EQ.2) XEXPT(2) = XTEMPR(1)
      KEYPNT = KEYPNT + NWRDS(I)
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XEXPT(3) = XTEMPD
      IF (I.EQ.2) XEXPT(3) = XTEMPR(1)
      KEYPNT = KEYPNT + NWRDS(I)
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XEXPT(4) = XTEMPD
      IF (I.EQ.2) XEXPT(4) = XTEMPR(1)
C                                       Date
      KEYPNT = LOCS(11)
      I = KEYTYP(11)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XDATE = XTEMPD
      IF (I.EQ.2) XDATE = XTEMPR(1)
C                                       Time
      KEYPNT = LOCS(12)
      I = KEYTYP(12)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XTIME = XTEMPD
      IF (I.EQ.2) XTIME = XTEMPR(1)
C                                       Cycle length
      KEYPNT = LOCS(13)
      I = KEYTYP(13)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XCYCL = XTEMPD
      IF (I.EQ.2) XCYCL = XTEMPR(1)
C                                       Sample rate
      KEYPNT = LOCS(14)
      I = KEYTYP(14)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XSAMP = XTEMPD
      IF (I.EQ.2) XSAMP = XTEMPR(1)
C                                       Epoch
      KEYPNT = LOCS(15)
      I = KEYTYP(15)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XEPOC = XTEMPD
      IF (I.EQ.2) XEPOC = XTEMPR(1)
C                                       RA
      KEYPNT = LOCS(16)
      I = KEYTYP(16)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XERA = XTEMPD
      IF (I.EQ.2) XERA = XTEMPR(1)
C                                       Declination
      KEYPNT = LOCS(17)
      I = KEYTYP(17)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XEDEC = XTEMPD
      IF (I.EQ.2) XEDEC = XTEMPR(1)
C                                       Beam width
      KEYPNT = LOCS(18)
      I = KEYTYP(18)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XBMSZ = XTEMPD
      IF (I.EQ.2) XBMSZ = XTEMPR(1)
C                                       Off scan
      KEYPNT = LOCS(19)
      I = KEYTYP(19)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XOFFS = XTEMPD
      IF (I.EQ.2) XOFFS = XTEMPR(1)
C                                       Velocity
      KEYPNT = LOCS(20)
      I = KEYTYP(20)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XVEL = XTEMPD
      IF (I.EQ.2) XVEL = XTEMPR(1)
C                                       Velocity defination
      KEYPNT = LOCS(21)
      XVELD = '        '
      IF (KEYPNT.GT.0) THEN
         CALL H2CHR (8, I, RECH(KEYPNT), XVELD)
         KBAD = 21
         IF (KEYTYP(21).NE.3) GO TO 110
         END IF
C                                       PTCONn
      KEYPNT = LOCS(22)
      I = KEYTYP(22)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XPTCN1 = XTEMPD
      IF (I.EQ.2) XPTCN1 = XTEMPR(1)
      KEYPNT = LOCS(23)
      I = KEYTYP(23)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XPTCN2 = XTEMPD
      IF (I.EQ.2) XPTCN2 = XTEMPR(1)
      KEYPNT = LOCS(24)
      I = KEYTYP(24)
      XTEMPD = 0.0D0
      IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
      IF (I.EQ.1) XPTCN3 = XTEMPD
      IF (I.EQ.2) XPTCN3 = XTEMPR(1)
      GO TO 120
C                                       Error with type
 110     WRITE (MSGTXT,1110) KEYGEN(KBAD)
         IRET = 3
         GO TO 990
C                                       Determine number of receivers
 120  NRCX = NUMCOL - 2
      NGET = 7
C                                       Receiver specific keywords
      DO 200 IRCX = 1,NRCX
         WRITE (CHTMP,1200) IRCX
C                                       Set receiver keywords
         DO 150 LOOP = 1,NGET
            KEYW(LOOP) = KEYRCX(LOOP)
C                                       Add rcx. number.
            KEYW(LOOP)(NCPRCX(LOOP):NCPRCX(LOOP)+1) = CHTMP(1:2)
 150        CONTINUE
C                                       Fetch values
         CALL TABKEY ('READ', KEYW, NGET, ITBUFF, LOCS, RECORD,
     *      KEYTYP, IERR)
         IF ((IERR.GT.0) .AND. (IERR.LE.20)) THEN
C                                       Error with TABKEY
            WRITE (MSGTXT,1100) IERR
            IRET = 2
            GO TO 990
            END IF
C                                       Frequency
         KEYPNT = LOCS(1)
         I = KEYTYP(1)
         XTEMPD = 0.0D0
         IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
         IF (I.EQ.1) XFREQ(IRCX) = XTEMPD
         IF (I.EQ.2) XFREQ(IRCX) = XTEMPR(1)
C                                       T Cal
         KEYPNT = LOCS(2)
         I = KEYTYP(2)
         XTEMPD = 0.0D0
         IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
         IF (I.EQ.1) XTCAL(IRCX) = XTEMPD
         IF (I.EQ.2) XTCAL(IRCX) = XTEMPR(1)
C                                       Polarization
         KEYPNT = LOCS(3)
         XPOLR(IRCX) = '        '
         IF (KEYPNT.GT.0) THEN
            CALL H2CHR (8, 1, RECH(KEYPNT), XPOLR(IRCX))
            KBAD = 3
            IF (KEYTYP(3).NE.3) GO TO 220
            END IF
C                                       Rho
         KEYPNT = LOCS(4)
         I = KEYTYP(4)
         XTEMPD = 0.0D0
         IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
         IF (I.EQ.1) XRHO(IRCX) = XTEMPD
         IF (I.EQ.2) XRHO(IRCX) = XTEMPR(1)
C                                       Theta
         KEYPNT = LOCS(5)
         I = KEYTYP(5)
         XTEMPD = 0.0D0
         IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
         IF (I.EQ.1) XTHETA(IRCX) = XTEMPD
         IF (I.EQ.2) XTHETA(IRCX) = XTEMPR(1)
C                                       Cal volts
         KEYPNT = LOCS(6)
         I = KEYTYP(6)
         XTEMPD = 0.0D0
         IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
         IF (I.EQ.1) XCALV(IRCX) = XTEMPD
         IF (I.EQ.2) XCALV(IRCX) = XTEMPR(1)
C                                       Bandwidth/freq separation
         KEYPNT = LOCS(7)
         I = KEYTYP(7)
         XTEMPD = 0.0D0
         IF (KEYPNT.GT.0) CALL COPY (NWRDS(I), RECI(KEYPNT), XTEMPR)
         IF (I.EQ.1) XBW(IRCX) = XTEMPD
         IF (I.EQ.2) XBW(IRCX) = XTEMPR(1)
 200     CONTINUE
      GO TO 250
C                                       Error with type
 220     WRITE (MSGTXT,1110) KEYW(KBAD)
         IRET = 3
         GO TO 990
 250  CONTINUE
C                                       Fix errors from tape, these
C                                       will be time variable!
C                                       General:
C                                       Sample rate wrong
      XSAMP = XCYCL
C                                       Convert ORIENT to rad
      XORI = XORI * 1.74533E-2
C                                       Receiver values
      DO 290 IRCX = 1,NRCX
C                                       RHO, THETA leave in rad.
C                                       Frequencies are bad, use TABED
C                                       to fix 1 and:
         XFREQ(IRCX) = XFREQ(1)
C                                       Band widths are bad, use TABed
C                                       to fix 1 and:
         XBW(IRCX) = XBW(1)
C                                       Convert polarization to upper
C                                       case.
         CALL CHLTOU (8, XPOLR(IRCX))
 290     CONTINUE
C                                       Consistency checks
C                                       Check table name
      DO 300 ITABTY = 1,NKTAB
         TABTYP = ITABTY
         IF (KNOTAB(ITABTY)(1:16).EQ.TABNAM(1:16)) GO TO 310
 300     CONTINUE
C***??? KLUDGE
      TABTYP = 1
      GO TO 310
C                                       Set pointers etc.
C                                       Table = 93M CONT (1)
 310  IF (TABTYP.EQ.1) THEN
         DOFREQ = .FALSE.
         DOTIME = .TRUE.
         KPARM = 2
         NUMST = 2
         NUMBM = (NCOL-2) / 2
         NUMFQ = 1
         NUMIF = 1
         KLENG = KPARM + NUMST * NUMBM * 3
         LCOR0 = -1
         KLOCS = NUMBM * 3
         KLOCB = 0
         KLOCF = -1
         KLOCIF = -1
         KINCS = - NUMBM * 3
         KINCB = 3
         KINCF = KLENG
         KINCIF = KLENG
         KLOCR = 0
         KLOCD = 1
         EXPT1N = 0.295
         EXPT2N = -0.4221
         END IF
C                                       Receiver parameters
      DO 350 IRCX = 1,NRCX
         LRCVR(IRCX) = TAN (XRHO(IRCX)) * SIN (XORI + XTHETA(IRCX))
         MRCVR(IRCX) = TAN (XRHO(IRCX)) * COS (XORI + XTHETA(IRCX))
         IF (ABS (XCALV(IRCX)).LE.1.0E-10) XCALV(IRCX) = 1.0
         RCXCAL(IRCX) = XTCAL(IRCX) / XCALV(IRCX)
         RCXAVG(IRCX) = 0.0
 350     CONTINUE
C                                       Other parameters
C                                       Pointing
      PTCON3 = XPTCN3
      EXPT1 = XEXPT(1)
      EXPT2 = XEXPT(2)
C                                       Epoch
      EPOCH = XEPOC
C                                       Date, time
      IYR = XDATE
      XDATE = (XDATE - IYR) * 100.0D0
      IMN = XDATE
      IDAY = (XDATE -IMN) * 100.0D0 + 0.5D0
      IF (IYR.LE.40) IYR = IYR + 2000
      IF (IYR.LE.200) IYR = IYR + 1900
      WRITE (REFDAT,1350) IYR, IMN, IDAY
      CALL JULDAY (REFDAT, JD)
      IF (JDREF.LE.1.0D0) JDREF = JD
      IDAY = JD - JDREF
      TSTART = (XTIME / 24.0D0) + IDAY
      TINCR = XSAMP / 86400.0
C                                       Position
      RATAB = XERA
      DECTAB = XEDEC
C                                       Frequency
      FRQTAB = XFREQ(1)
      FRQINC = XBW(1)
C                                       Scan info
      SCAN = XSCAN + 0.1
      WRITE (MSGTXT,1351) SCAN
      CALL MSGWRT (6)
C                                       Misc.
      BEMSIZ = XBMSZ
      CURREC = 1
      CURBEM = 1
      NUMRCX = NRCX
C                                       Antenna info
      NANT = 1
      DO 390 LOOP = 2,NKANT
         IDANT = LOOP
         IF (TELESC.EQ.KNTELE(LOOP)) GO TO 400
 390     CONTINUE
C                                       Unknown antenna
         WRITE (MSGTXT,1390) TELESC
         CALL MSGWRT (6)
         IDANT = 1
C                                       Antenna position, name
 400     ANTLOC(1,1) = KNLOC(1,IDANT)
         ANTLOC(2,1) = KNLOC(2,IDANT)
         ANTLOC(3,1) = KNLOC(3,IDANT)
         ANTNAM = TELESC
         FL = KNTELP(1,IDANT)
         IF (ABS (FL).LT.1.0E-5) FL = 1.0
         BDF = KNTELP(2,IDANT)
         LFMIN = KNTELP(3,IDANT)
         LFMAX = KNTELP(4,IDANT)
C                                       Read table
      NUMDAT = (KLENG - KPARM) / 3
      INDEX = 1
      NDATA = (KLENG - KPARM) / 3
      DO 500 RECNO = 1,NUMROW
         CALL TABIO ('READ', 0, RECNO, RECORD, ITBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1400) IRET
            GO TO 990
            END IF
         IF ((INDEX+KPARM+NDATA).GT.HUGE) THEN
C                                       Too much data
            WRITE (MSGTXT,1399)
            CALL MSGWRT (8)
            NUMROW = RECNO - 1
            GO TO 510
            END IF
C                                       "random" parameters
         DO 420 LOOP = 1,KPARM
            MUTHA(INDEX) = RECORD(LOOP)
            INDEX = INDEX + 1
 420        CONTINUE
C                                       Regular data, expand to 3 words
         DO 440 LOOP = 1,NDATA
C                                       Calibrate
            MUTHA(INDEX) = RECORD(LOOP+KPARM) * RCXCAL(LOOP)
C                                       Sum
            IF (AVGSUB) RCXAVG(LOOP) = RCXAVG(LOOP) + MUTHA(INDEX)
            MUTHA(INDEX+1) = 0.0
            MUTHA(INDEX+2) = 1.0
            INDEX = INDEX + 3
 440        CONTINUE
 500     CONTINUE
C                                       Close table
 510  CALL TABIO ('CLOS', 0, RECNO, RECORD, ITBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1400) IRET
         GO TO 990
         END IF
C                                       Average temperatures
      DO 550 IRCX = 1,NUMRCX
         RCXAVG(IRCX) = RCXAVG(IRCX) / NUMROW
 550     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      WRITE (MSGTXT,1990) TABVER
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDTRED: TABINI ERROR',I3,' OPENING INPUT TABLE')
 1001 FORMAT ('SDTRED: TABLE TYPE CHANGED, NEW TYPE:')
 1002 FORMAT ('       ',A)
 1003 FORMAT ('   OLD TYPE WAS:')
 1100 FORMAT ('SDTRED: TABKEY ERROR ',I3,' READING KEYWORDS')
 1110 FORMAT ('SDTRED: WRONG DATA TYPE FOR KEYWORD =',A8)
 1200 FORMAT (I2.2)
 1350 FORMAT (I4.4,2I2.2)
 1351 FORMAT ('Processing scan number ',I9)
 1390 FORMAT ('SDTRED: UNKNOWN TELESCOPE: ',A8)
 1399 FORMAT ('SDTRED: TRUNCATING SCAN')
 1400 FORMAT ('SDTRED: ERROR',I4,' READING TABLE')
 1990 FORMAT ('SDTRED: ERROR OCCURED READING TABLE NO. ',I6)
      END
      SUBROUTINE SDTCS (TIME, SUBA, RA, DEC, NOBEAM, NUMPOL, NUMIF,
     *   DISK, CNO, CATBLK, BUFFER, IRET)
C-----------------------------------------------------------------------
C   Routine to create/initialize a single dish calibration file (CS)
C   Table opened and closed each call.
C    Input:
C     TIME      R     Time (days)
C     SUBA      I     Subarray number.
C     RA(*)     R     RA (deg) 1 per beam
C     DEC(*)    R     Declination  (deg) 1 per beam
C     NOBEAM    I     Number of beams.
C     NUMPOL    I     Number of polarizations
C     NUMIF     I     Number of IFs
C     DISK      I     Disk number of file
C     CNO       I     Catalog slot number of file
C     CATBLK(*) I     Catalog header block
C    Output:
C     BUFFER(*) I     I/O buffer
C     IRET      I     Return code, 0=>OK, else TABCAL or CALINI error.
C-----------------------------------------------------------------------
      INTEGER   SUBA, DISK, CNO, CATBLK(256), BUFFER(*), IRET
      INTEGER   NOBEAM, NUMPOL, NUMIF
      REAL      TIME, RA(*), DEC(*)
      INTEGER   VER, LUN, CSKOLS(13), CSNUMV(13), IBEAM, MUMBEM,
     *   MUMPOL, MUMIF, ICSRNO
      INCLUDE 'INCS:PUVD.INC'
      REAL      FAC(2,MAXIF), OFF(2,MAXIF), RAO(2,MAXIF), DECO(2,MAXIF)
      DATA VER /1/
      DATA        FAC,                 OFF
     *   /MAXIF*1.0,MAXIF*1.0, MAXIF*0.0,MAXIF*0.0/
      DATA        RAO,                 DECO
     *   /MAXIF*0.0,MAXIF*0.0, MAXIF*0.0,MAXIF*0.0/
C-----------------------------------------------------------------------
C                                       Open/create
      LUN = 26
      MUMBEM = NOBEAM
      MUMPOL = NUMPOL
      MUMIF = NUMIF
      CALL CSINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICSRNO, CSKOLS, CSNUMV, MUMBEM, MUMPOL, MUMIF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Do beams
      DO 200 IBEAM = 1,NOBEAM
         CALL TABCS ('WRIT', BUFFER, ICSRNO, CSKOLS, CSNUMV, MUMPOL,
     *      TIME, RA(IBEAM), DEC(IBEAM), IBEAM, SUBA, FAC, OFF,
     *      RAO, DECO, IRET)
         IF (IRET.NE.0) GO TO 999
 200     CONTINUE
C                                       Close
      CALL TABCS ('CLOS', BUFFER, ICSRNO, CSKOLS, CSNUMV, MUMPOL,
     *   TIME, RA, DEC, IBEAM, SUBA, FAC, OFF, RAO, DECO, IRET)
C
 999  RETURN
      END
      SUBROUTINE SDTNX (VSTART, VEND, TIME, DT, IDSOU, SUBA,
     *   DISK, CNO, CATBLK, FRQSEL, BUFFER, IRET)
C-----------------------------------------------------------------------
C   Routine to create/initialize an Index (NX) table.
C   Table opened and closed each call.
C    Input:
C     VSTART    I     First visibility number
C     VEND      I     Highest visibility number
C     TIME      R     Center time (days)
C     DT        R     Time interval (days)
C     SUBA      I     Subarray number.
C     IDSOU     I     Source ID.
C     DISK      I     Disk number of file
C     CNO       I     Catalog slot number of file
C     CATBLK(*) I     Catalog header block
C     FRQSEL    I     Freq id number
C    Output:
C     BUFFER(*) I     I/O buffer
C     IRET      I     Return code, 0=>OK, else TABCAL or CALINI error.
C-----------------------------------------------------------------------
      INTEGER   VSTART, VEND, IDSOU, SUBA, DISK, CNO, CATBLK(256),
     *   BUFFER(*), FRQSEL, IRET
      REAL      TIME, DT
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, LUN, NXKOLS(MAXNXC), NXNUMV(MAXNXC), INXRNO
      DATA VER /1/
C-----------------------------------------------------------------------
C                                       Open/create
      LUN = 26
      CALL NDXINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   INXRNO, NXKOLS, NXNUMV, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Make entry
      CALL TABNDX ('WRIT', BUFFER, INXRNO, NXKOLS, NXNUMV,
     *   TIME, DT, IDSOU, SUBA, VSTART, VEND, FRQSEL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Close
      CALL TABNDX ('CLOS', BUFFER, INXRNO, NXKOLS, NXNUMV,
     *   TIME, DT, IDSOU, SUBA, VSTART, VEND, FRQSEL, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SDTMWF
C-----------------------------------------------------------------------
C   Routine to do median window filtering on data in array MUTHA.
C   The length of the window is APARM(5) points.
C    Input from common:
C     APARM(*)     R    (5) Number of points to be used for the median
C                       window, 0=> none.
C     NUMROW       I    Number of rows in the table.
C     NUMCOL       I    Number of columns in table.
C     NUMRCX       I    Number of receivers
C     KPARM        I    Number of words of "random" data before data.
C     KLENG        I    Length of table record as kept in common.
C    Input/output in common:
C     MUTHA(huge)  R    Data array, See SDTRED for a description of the
C                       data ordering.  Both value and offset corrected.
C     TBUFF(*)     R    Work array
C   Output in common:
C     RCXAVG(*)    R    Receiver average value, set to 0.0 if filtering
C                       done.
C-----------------------------------------------------------------------
      INTEGER   LOOP, NCOLDO, POINT, COLOOP, NGUARD,
     *   IP, MAXN, XXDIM, IAPT, INDX, LIM1, LIM2
C                                       MAXN = max. n
      PARAMETER (MAXN = 1024)
      PARAMETER (XXDIM = MAXN+2)
      REAL      CORR, A(2,MAXN), WK(2,XXDIM), ADD
      INTEGER    KEY1, KEY2, IER, JP(XXDIM), IDROP, IADD, NPTS,
     *   N, NP2, LEN, NSORT
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       See if requested
      IF (APARM(5).LE.0.0) GO TO 999
C                                       Number of column to do, etc.
      NCOLDO = (KLENG - KPARM) / 3
      N = APARM(5) + 0.5
C                                       Make sure N is even
      N = (N / 2) * 2
      IF (N.GT.MAXN) N = MAXN
      NP2 = N + 2
      APARM(5) = N
      NGUARD = N / 2
      KEY1 = 1
      KEY2 = 1
      LEN = 2
C                                       Initialize pointer
      POINT = KPARM + 1
C                                       Loop over columns
      DO 500 COLOOP = 1,NCOLDO
C                                       Initialize sorted list
         IP = POINT + (COLOOP-1) * 3
         IAPT = 1
         NPTS = 0
         DO 100 LOOP = 1,N
            IF (MUTHA(IP+2).GT.0.0) THEN
               A(1,IAPT) = MUTHA(IP)
               A(2,IAPT) = LOOP
               NPTS = NPTS + 1
               IAPT = IAPT + 1
               END IF
            IP = IP + KLENG
 100        CONTINUE
C                                       Sort
         NSORT = NPTS
         CALL OSORT (A, NSORT, NP2, KEY1, KEY2, LEN, WK, JP, IER)
C                                       Compute median
         IDROP = -1
         IADD = -1
         CALL MEDIAN (IDROP, IADD, ADD, A, NPTS, CORR)
C                                       Correct data
C                                       First window half width
         IP = POINT + (COLOOP-1) * 3
      INCLUDE 'INCS:ZVND.INC'
         DO 120 LOOP = 1,NGUARD
            MUTHA(IP) = MUTHA(IP) - CORR
            MUTHA(IP+1) = MUTHA(IP+1) + CORR
            IP = IP + KLENG
 120        CONTINUE
C                                       Middle section
         LIM1 = NGUARD + 1
         LIM2 = NUMROW - NGUARD
         DO 160 LOOP = LIM1,LIM2
            INDX = IP + NGUARD * KLENG
            IF (MUTHA(INDX+2).GT.0.0) THEN
               IADD = LOOP + NGUARD
               ADD = MUTHA(INDX)
            ELSE
C                                       Don't add bad point
               IADD = -1
               END IF
C                                       Next median
            IDROP = LOOP - NGUARD
            CALL MEDIAN (IDROP, IADD, ADD, A, NPTS, CORR)
            MUTHA(IP) = MUTHA(IP) - CORR
            MUTHA(IP+1) = MUTHA(IP+1) + CORR
            IP = IP + KLENG
 160        CONTINUE
C                                       Last window half width
         LIM1 = LIM2 + 1
         LIM2 = NUMROW
      INCLUDE 'INCS:ZVND.INC'
         DO 180 LOOP = LIM1,LIM2
            MUTHA(IP) = MUTHA(IP) - CORR
            MUTHA(IP+1) = MUTHA(IP+1) + CORR
            IP = IP + KLENG
 180        CONTINUE
C                                       End of column loop
 500     CONTINUE
C                                       Zero receiver averages
      DO 600 LOOP = 1,NUMRCX
         RCXAVG(LOOP) = 0.0
 600     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MEDIAN (IDROP, IADD, ADD, A, NPTS, CORR)
C-----------------------------------------------------------------------
C   Routine to manage a sorted list and compute a median
C    Inputs:
C     IDROP     I    Highest index number to drop. .le. 0 =>none.
C     IADD      I    Current index number to add. .le. 0 =>none.
C     ADD       R    Value at index=IADD.
C    Input/output:
C     A(2,*)    R    Value, index sorted by value (descending)
C     NPTS      I    Number of value/index pairs in A
C    Output:
C     CORR      R    The median value
C-----------------------------------------------------------------------
      INTEGER   IDROP, IADD, NPTS
      REAL      ADD, A(2,*), CORR
      INTEGER   LOOP, LIM, IP, INDXA, INDXD, IOFF
      REAL      XDROP
C-----------------------------------------------------------------------
      LIM = NPTS
      XDROP = IDROP
C                                       Take care of sorted list:
      IF ((IADD.GT.0) .OR. (IDROP.GT.0)) THEN
C                                       Find point to drop and where to
C                                       insert new one.
         INDXD = 0
         INDXA = 0
         DO 20 LOOP = 1,LIM
            IF ((A(2,LOOP)-XDROP) .LE. 0.3) INDXD = LOOP
            IF (A(1,LOOP).GT.ADD) INDXA = LOOP
 20         CONTINUE
C                                       Trap no add or no drop cases
         IF (IADD.LE.0) THEN
            INDXA = LIM
         ELSE
            NPTS = NPTS + 1
            END IF
         IF ((IDROP.LE.0) .OR. (INDXD.LE.0)) THEN
            INDXD = LIM + 1
         ELSE
            NPTS = NPTS - 1
            END IF
C                                       Shuffle
         IOFF = 0
         IF (INDXA.GT.INDXD) THEN
C                                       Drop
            LIM = INDXA - 1
      INCLUDE 'INCS:ZVND.INC'
            DO 40 LOOP = INDXD,LIM
               A(1,LOOP) = A(1,LOOP+1)
               A(2,LOOP) = A(2,LOOP+1)
 40            CONTINUE
            END IF
C                                       Pull up
         IF (INDXA+1.LT.INDXD) THEN
            IOFF = 1
            IP = INDXD - 1
            LIM = INDXA + 2
      INCLUDE 'INCS:ZVND.INC'
            DO 60 LOOP = LIM,INDXD
              A(1,IP+1) = A(1,IP)
              A(2,IP+1) = A(2,IP)
              IP = IP - 1
 60           CONTINUE
            END IF
         IF (INDXA+1.EQ.INDXD) IOFF = 1
C                                       Insert new point
         A(1,INDXA+IOFF) = ADD
         A(2,INDXA+IOFF) = IADD
         END IF
C                                       Compute median from two center
C                                       points
      IF (NPTS.GT.1) THEN
         IP = NPTS / 2
         CORR = 0.5 * (A(1,IP) + A(1,IP+1))
      ELSE IF (NPTS.EQ.1) THEN
         CORR = A(1,1)
      ELSE
         CORR = 0.0
         END IF
         NPTS = MAX (0, NPTS)
C
 999  RETURN
      END
      SUBROUTINE SDTSPL
C-----------------------------------------------------------------------
C   Routine to determine and remove a cubic spline smoothing function.
C   The spline is fitted to the median value of a run of data APARM(3)
C   points long using a smoothing parameter of APARM(4).
C    Input from common:
C     APARM(*)     R    (3) Number of points to be used for the median
C                       window, 0=> none. Max. 1000
C                       (4) Smoothing parameter (0=>1)
C     NUMROW       I    Number of rows in the table.
C     NUMCOL       I    Number of columns in table.
C     NUMRCX       I    Number of receivers
C     KPARM        I    Number of words of "random" data before data.
C     KLENG        I    Length of table record as kept in common.
C    Input/output in common:
C     MUTHA(huge)  R    Data array, See SDTRED for a description of the
C                       data ordering.  Both value and offset corrected.
C   Output in common:
C     RCXAVG(*)    R    Receiver average value, set to 0.0 if filtering
C                       done.
C-----------------------------------------------------------------------
      INTEGER   LOOP, NCOLDO, POINT, COLOOP, NBLOCK, LBLOCK,
     *   BLOOP, IP, LENGTH, MAXN, NLEFT
C                                       MAXN = max. N
      PARAMETER (MAXN = 1024)
      REAL      CORR, H, TNODE(MAXN), G(MAXN), WGS(MAXN), RHO,
     *   GSMO(MAXN), B(MAXN), C(MAXN), D(MAXN), A(2,MAXN),
     *   T, TN, BN, CN, DN, GN, TT, TT2, TT3, ADD
      INTEGER   IDROP, IADD, NPTS, N
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       See if requested
      IF (APARM(3).LE.0.0) GO TO 999
C                                       Number of column to do, etc.
      NCOLDO = (KLENG - KPARM) / 3
      N = APARM(3) + 0.5
      RHO = APARM(4)
      IF (N.GT.MAXN) N = MAXN
      IF (RHO.LE.0.0) RHO = 1.0
      APARM(3) = N
      APARM(4) = RHO
      LBLOCK = N
      NBLOCK = NUMROW / LBLOCK
C                                       Initialize pointer
      POINT = KPARM + 1
C                                       Loop over columns
      DO 500 COLOOP = 1,NCOLDO
         LENGTH = LBLOCK
         NLEFT = NUMROW
C                                       Loop done column processing
C                                       blocks.
         IP = POINT + (COLOOP-1) * 3
         IDROP = -1
         DO 200 BLOOP = 1,NBLOCK
C                                       Need block median
            LENGTH = MIN (LBLOCK, NLEFT)
            NPTS = 0
            DO 100 LOOP = 1,LENGTH
               IADD = LOOP
               ADD = MUTHA(IP)
               IF (MUTHA(IP+2).GT.0.0)
     *            CALL MEDIAN (IDROP, IADD, ADD, A, NPTS, CORR)
               IP = IP + KLENG
 100           CONTINUE
            G(BLOOP) = CORR
            WGS(BLOOP) = 1.0
            TNODE(BLOOP) = ((BLOOP-1.0) * LBLOCK) + (LBLOCK * 0.5)
            NLEFT = NLEFT - LENGTH
 200        CONTINUE
C                                       Fit spline
         N = NBLOCK
         H = LBLOCK
         CALL DCSSMO (H, N, TNODE, G, WGS, RHO, GSMO, B, C, D)
C                                       Correct
         IP = POINT + (COLOOP-1) * 3
         T = 0.0
         NLEFT = NUMROW
         DO 400 BLOOP = 1,NBLOCK
            TN = TNODE(BLOOP)
            GN = GSMO(BLOOP)
            BN = B(BLOOP)
            CN = C(BLOOP)
            DN = D(BLOOP)
            LENGTH = MIN (LBLOCK, NLEFT)
C                                       Compute spline values and apply
            DO 300 LOOP = 1,LENGTH
               TT = (T + LOOP) - TN
               TT2 = TT * TT
               TT3 = TT2 * TT
               CORR = GN + BN * TT + CN * TT2 + DN * TT3
               MUTHA(IP) = MUTHA(IP) - CORR
               MUTHA(IP+1) = MUTHA(IP+1) + CORR
               IP = IP + KLENG
 300           CONTINUE
            T = T + 1.0 * LENGTH
            NLEFT = NLEFT - LENGTH
 400        CONTINUE
C                                       End of column loop
 500     CONTINUE
C                                       Zero receiver averages
      DO 600 LOOP = 1,NUMRCX
         RCXAVG(LOOP) = 0.0
 600     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE DCSSMO(H, N, TNODE, G, WGS, RHO, GSMO, B, C, D)
C-----------------------------------------------------------------------
C  THIS SUBROUTINE COMPUTES THE DISCRETE NATURAL CUBIC
C  SPLINE DEFINED ON THE INTERVAL (TNODE(1),TNODE(N)) WHICH
C  SMOOTHS THROUGH THE DATA (TNODE(I),G(I)),I=1,2,...,N.
C  N MUST BE 2 OR GREATER. THE NODES MUST SATISFY TNODE(I)
C  .LT.TNODE(I+1). THE SOLUTION S(T) FOR T IN THE INTERVAL
C  (TNODE(I),TNODE(I+1)) IS GIVEN BY:
C     S(T)=GSMO(I)+B(I)*(T-TNODE(I))+
C             C(I)*(T-TNODE(I))**2+D(I)*(T-TNODE(I))*
C
C  INPUT  PARAMETERS(NONE OF THE INPUT PARAMETERS ARE CHANGED
C         BY THIS SUBROUTINE)
C
C  H     - THE STEP SIZE USED FOR THE DISCRETE CUBIC SPLINE
C  N     - NUMBER OF NODES (TNODE) AND DATA VALUES(G)
C  TNODE - REAL ARRAY CONTAINING THE NODES (TNODE(I).LT.
C          TNODE(I+1)).
C  G     - REAL ARRAY CONTAINING THE DATA VALUES.
C  WGS   - REAL ARRAY CONTAINING THE WEIGHTS WGS(I)
C          CORRESPONDING TO THE DATA (TNODE(I),G(I)).
C  RHO   - SIMPLE REAL VARIABLE CONTAINING THE POSITIVE
C          PARAMETER FOR VARYING THE SMOOTHNESS OF THE FIT.
C          IF RHO IS SMALL SMOOTHNESS IS EMPHASIZED.
C          IF RHO IS LARGE DATA FITTING IS EMPHASIZED.
C
C  OUTPUT PARAMETERS
C
C  GSMO  - REAL ARRAY CONTAINING THE SMOOTHED VALUES OF
C          THE DATA G(I),I=1,2,....,N.
C  B     - REAL ARRAY CONTAINING THE COEFFICIENTS B(I) FOR
C          THE TERMS (T-TNODE(I)).
C  C     - REAL ARRAY CONTAINING THE COEFFICIENTS C(I) FOR
C          THE TERMS (T-TNODE(I))**2.
C  D     - REAL ARRAY CONTAINING THE COEFFICIENTS D(I) FOR
C          THE TERMS (T-TNODE(I))**3.
C   From: C. S. Duris, ACMTOMS 6, no. 1, p 92, 1980.  Algorithm 547
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      TNODE(N), G(N), WGS(N), GSMO(N), B(N), C(N), D(N), H,
     *   RHO
      INTEGER   N1, N2, N3, I, I1, I2, K, K1, K2, K3, J
      REAL      H2, H3, R6, HI3, HI4, HI5, ETA3, ETA4, P,
     *   BETA3, BETA4, BETA5, H2DHI, EPS3, EPS4, HK1, HK2
C-----------------------------------------------------------------------
      IF (N.EQ.2) GO TO 180
      N1 = N - 1
      N2 = N1 - 1
      N3 = N2 - 1
C  THE RIGHT HAND SIDE OF THE LINEAR SYSTEM FOR THE
C  C(I)'S WILL NOW BE CONSTRUCTED.
      DO 10 I=1,N
        C(I) = G(I)
   10 CONTINUE
      DO 20 I=1,N1
        C(I) = (C(I+1)-C(I))/(TNODE(I+1)-TNODE(I))
   20 CONTINUE
      DO 30 I=1,N2
        C(I) = 3.0*(C(I+1)-C(I))
   30 CONTINUE
C  THE RIGHT HAND SIDE IS NOW IN ARRAY C.
C  THE P.D. 5 BANDED SYMMETRIC MATRIX WILL NOW BE CONSTRUCTED.
C  THE THREE NEEDED DIAGONALS WILL BE STORED IN ARRAYS
C  GSMO,B,D.
      H2 = H*H
      H3 = H2*H
      R6 = 6.0*H3/RHO
      HI3 = TNODE(2) - TNODE(1)
      HI4 = TNODE(3) - TNODE(2)
      ETA3 = HI3 + HI3 + H2/HI3
      BETA3 = R6/(WGS(1)*HI3)
      BETA4 = R6/(WGS(2)*HI3*HI4)
      EPS3 = (BETA3+BETA4*HI4)/HI3
      H2DHI = H2/HI4
      ETA4 = HI4 + HI4 + H2DHI
      IF (N.EQ.3) GO TO 60
      HI5 = TNODE(4) - TNODE(3)
      BETA5 = R6/(WGS(3)*HI4*HI5)
      EPS4 = (BETA4*HI3+BETA5*HI5)/HI4
      GSMO(1) = ETA3 + ETA4 + BETA4 + BETA4 + EPS3 + EPS4
      P = H2DHI + BETA4 + BETA5 + EPS4
      B(1) = HI4 - P
      IF (N.EQ.4) GO TO 50
      DO 40 I=2,N3
        HI3 = HI4
        HI4 = HI5
        HI5 = TNODE(I+3) - TNODE(I+2)
        ETA3 = ETA4
        H2DHI = H2/HI4
        ETA4 = HI4 + HI4 + H2DHI
        BETA3 = BETA4
        BETA4 = BETA5
        BETA5 = R6/(WGS(I+2)*HI4*HI5)
        EPS3 = EPS4
        EPS4 = (BETA4*HI3+BETA5*HI5)/HI4
        D(I-1) = BETA4
        P = H2DHI + BETA4 + BETA5 + EPS4
        B(I) = HI4 - P
        GSMO(I) = ETA3 + ETA4 + BETA4 + BETA4 + EPS3 + EPS4
   40 CONTINUE
   50 HI3 = HI4
      HI4 = HI5
      ETA3 = ETA4
      ETA4 = HI4 + HI4 + H2/HI4
      BETA4 = BETA5
      EPS3 = EPS4
   60 BETA5 = R6/(WGS(N)*HI4)
      EPS4 = (BETA4*HI3+BETA5)/HI4
      GSMO(N2) = ETA3 + ETA4 + BETA4 + BETA4 + EPS3 + EPS4
C  THE P.D. 5 BANDED SYMMETRIC MATRIX IS COMPLETE.
C  THE SYSTEM OF LINEAR EQUATION WILL NOW BE SOLVED FOR THE
C  C(I)'S.
      IF (N.GT.3) GO TO 70
      C(1) = C(1)/GSMO(1)
      GO TO 150
   70 IF (N.GT.4) GO TO 80
      C(1) = (C(1)*GSMO(2)-C(2)*B(1))/(GSMO(1)*GSMO(2)-B(1)**2)
      C(2) = (C(2)-C(1)*B(1))/GSMO(2)
      GO TO 150
C  THIS SOLVE THE 5 BANDED SYSTEM WHEN K=N-2.GT.3.
   80 K = N2
      K1 = K - 1
      K2 = K1 - 1
      K3 = K2 - 1
C  THE 5 BANDED MATRIX WILL NOW BE FACTORED.
      B(1) = B(1)/GSMO(1)
      D(1) = D(1)/GSMO(1)
      P = GSMO(1)*B(1)
      GSMO(2) = GSMO(2) - P*B(1)
      B(2) = (B(2)-P*D(1))/GSMO(2)
      IF (K.EQ.3) GO TO 110
      D(2) = D(2)/GSMO(2)
      IF (K.EQ.4) GO TO 100
      DO 90 I=3,K2
        I1 = I - 1
        I2 = I1 - 1
        P = GSMO(I1)*B(I1)
        GSMO(I) = GSMO(I) - GSMO(I2)*(D(I2)**2) - P*B(I1)
        B(I) = (B(I)-P*D(I1))/GSMO(I)
        D(I) = D(I)/GSMO(I)
   90 CONTINUE
  100 P = GSMO(K2)*B(K2)
      GSMO(K1) = GSMO(K1) - GSMO(K3)*(D(K3)**2) - P*B(K2)
      B(K1) = (B(K1)-P*D(K2))/GSMO(K1)
  110 GSMO(K) = GSMO(K) - GSMO(K2)*(D(K2)**2) - GSMO(K1)*(B(K1)**2)
C  FACTORIZATION COMPLETE.
C  CARRY OUT FORWARD  AND BACKWARD SUBSTITUTION.
      C(2) = C(2) - B(1)*C(1)
      DO 120 I=3,K
        I1 = I - 1
        I2 = I - 2
        C(I) = C(I) - B(I1)*C(I1) - D(I2)*C(I2)
  120 CONTINUE
      DO 130 I=1,K
        C(I) = C(I)/GSMO(I)
  130 CONTINUE
      C(K1) = C(K1) - B(K1)*C(K)
      DO 140 I=2,K1
        J = K - I
        C(J) = C(J) - B(J)*C(J+1) - D(J)*C(J+2)
  140 CONTINUE
C  THE 5 BANDED SYSTEM HAS BEEN SOLVED.THE SOLUTION IS IN
C  ARRAY C. THE COEFFICIENTS GSMO, B, C, AND D WILL NOW BE
C  SET UP.
  150 C(N) = 0.0
      D(N) = 0.0
      C(N1) = C(N2)
      HK1 = TNODE(N) - TNODE(N1)
      D(N1) = -C(N1)/(3.0*HK1)
      GSMO(N) = G(N) + R6*D(N1)/WGS(N)
      IF (N.EQ.3) GO TO 170
      DO 160 I=2,N2
        K = N - I
        K1 = K + 1
        HK2 = HK1
        HK1 = TNODE(K1) - TNODE(K)
        C(K) = C(K-1)
        D(K) = (C(K1)-C(K))/(3.0*HK1)
        GSMO(K1) = G(K1) - R6*(D(K1)-D(K))/WGS(K1)
        B(K1) = (GSMO(K1+1)-GSMO(K1))/HK2 - HK2*(C(K1)+C(K1)+C(K1+1))/
     *    3.0
  160 CONTINUE
  170 C(1) = 0.0
      HK2 = HK1
      HK1 = TNODE(2) - TNODE(1)
      D(1) = (C(2)-C(1))/(3.0*HK1)
      GSMO(2) = G(2) - R6*(D(2)-D(1))/WGS(2)
      GSMO(1) = G(1) - R6*D(1)/WGS(1)
      B(2) = (GSMO(3)-GSMO(2))/HK2 - HK2*(C(2)+C(2)+C(3))/3.0
      B(1) = (GSMO(2)-GSMO(1))/HK1 - HK1*(C(1)+C(1)+C(2))/3.0
C  THE DISCRETE CUBIC SMOOTHING SPLINE IS NOW COMPLETE.
      RETURN
C  THE TRIVIAL CASE WHEN N=2 IS HANDLED HERE.
  180 GSMO(1) = G(1)
      GSMO(2) = G(2)
      B(1) = (G(2)-G(1))/(TNODE(2)-TNODE(1))
      C(1) = 0.0
      D(1) = 0.0
      RETURN
      END
      SUBROUTINE SDTZAP
C-----------------------------------------------------------------------
C   Routine detect and flag interference.
C   Interference is detected by subtracting the mean of the preceeding
C   and following point from each receiver value.  If more than APARM(8)
C   receivers show more than APARM(7) deviation then the time sample is
C   flagged.  This routine assumes that no data has been flagged
C   previously.
C    Input from common:
C     APARM(*)     R    (7) Max. deviation, 0=> just return.
C                       (8) Max. no. receivers exceeding (7)
C     NUMROW       I    Number of rows in the table.
C     NUMCOL       I    Number of columns in table.
C     NUMRCX       I    Number of receivers
C     KPARM        I    Number of words of "random" data before data.
C     KLENG        I    Length of table record as kept in common.
C    Input/output in common:
C     MUTHA(huge)  R    Data array, See SDTRED for a description of the
C                       data ordering.  Weights set to 0 for flagged
C                       data.
C-----------------------------------------------------------------------
      INTEGER   LOOP, POINT, MAXBAD, LIMIT, LOPRCX, IP, BADCNT, NUMFLG
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      REAL      DEV(MAXRCX), DEVMAX
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       See if requested
      IF (APARM(7).LE.1.0E-30) GO TO 999
      DEVMAX = APARM(7)
      MAXBAD = APARM(8) + 0.1
      LIMIT = NUMROW - 1
      NUMFLG = 0
C                                       Loop through time samples
      DO 500 LOOP = 2,LIMIT
         POINT = KPARM + 1 + (LOOP-1) * KLENG
C                                       Find deviations
         IP = POINT
         DO 100 LOPRCX = 1,NUMRCX
            DEV(LOPRCX) = MUTHA(IP) - (0.5 * (MUTHA(IP-KLENG) +
     *         MUTHA(IP+KLENG)))
            IP = IP + 3
 100        CONTINUE
C                                       Count bad ones
         BADCNT = 0
         DO 200 LOPRCX = 1,NUMRCX
            IF (DEV(LOPRCX).GT.DEVMAX) BADCNT = BADCNT + 1
 200        CONTINUE
         IF (BADCNT.GT.MAXBAD) THEN
C                                       Flag time sample
            IP = POINT + 2
            DO 300 LOPRCX = 1,NUMRCX
               MUTHA(IP) = 0.0
               IP = IP + 3
 300           CONTINUE
            NUMFLG = NUMFLG + 1
            END IF
C                                       End of data loop
 500     CONTINUE
C                                       Tell if flagged data
      IF (NUMFLG.GT.0) THEN
         WRITE (MSGTXT,1500) NUMFLG
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('SDTZAP:',I5,' time samples flagged due to interference')
      END
      SUBROUTINE BADRA
C-----------------------------------------------------------------------
C   Routine to detect bad RAs from the 300 ft control program and
C   replace them with nominal values.
C    Input from common:
C     BPARM(*)     R    (1) If > 0 then do correction.
C     NUMROW       I    Number of rows in the table.
C     KLENG        I    Length of table record as kept in common.
C     KLOCR        I    Offset of RA in table
C    Input/output in common:
C     MUTHA(huge)  R    Data array, See SDTRED for a description of the
C                       data ordering.  Weights set to 0 for flagged
C                       data.
C-----------------------------------------------------------------------
      INTEGER   LOOP, LIMIT, IP, BADCNT, INDXOK
      REAL      RAOK, DELTA, DELOK, DIFF
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       See if requested
      IF (BPARM(1).LE.1.0E-30) GO TO 999
      DELTA = TINCR * 360.0
      DELOK = 10.0 * DELTA
      LIMIT = NUMROW - 1
      BADCNT = 0
C                                       Find good RA
      IP = 1 + KLOCR
      DO 100 LOOP = 1,LIMIT
         DIFF = ABS ((MUTHA(IP+KLENG)-MUTHA(IP))-DELTA)
         IF (DIFF.GT.359.0) DIFF = DIFF - 360.0
         IF (DIFF.LE.DELOK) THEN
C                                       Found good RA
            RAOK = MUTHA(IP)
            INDXOK = LOOP
            GO TO 110
            END IF
         IP = IP + KLENG
 100     CONTINUE
C                                       No good data found
         WRITE (MSGTXT,1100)
         CALL MSGWRT (6)
         GO TO 999
 110     IP = 1 + KLOCR
C                                       Loop thru table
         DO 200 LOOP = 1,LIMIT
            DIFF = ABS ((MUTHA(IP+KLENG)-MUTHA(IP))-DELTA)
            IF (DIFF.GT.359.0) DIFF = DIFF - 360.0
            IF (DIFF.LE.DELOK) THEN
C                                       Good
               RAOK = MUTHA(IP)
               INDXOK = LOOP
            ELSE
C                                       Bad RA
               MUTHA(IP) = RAOK + (LOOP-INDXOK) * DELTA
               BADCNT = BADCNT + 1
               END IF
            IP = IP + KLENG
 200        CONTINUE
C                                       Last point
         IP = (NUMROW-1) * KLENG + 1 + KLOCR
         DIFF = ABS ((MUTHA(IP)-MUTHA(IP-KLENG))-DELTA)
         IF (DIFF.GT.359.0) DIFF = DIFF - 360.0
         IF (DIFF.LE.DELOK) THEN
            MUTHA(IP) = RAOK + (NUMROW-INDXOK) * DELTA
            BADCNT = BADCNT + 1
            END IF
      IF (BADCNT.GE.1) THEN
C                                       Tell if fixed RAs
         WRITE (MSGTXT,1200) BADCNT
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('NO GOOD RAS FOUND - THIS IS NOT GOOD')
 1200 FORMAT ('BADRA: Fixed ',I6,' bad RAs')
      END
      SUBROUTINE BADEC
C-----------------------------------------------------------------------
C   Routine to detect bad declinations from the 140 ft control program
C   and replace them with nominal values.  The 140 ft problem appears to
C   be that the Declinations of some samples are not updated IN TIME.
C   Input from common:
C     BPARM(*)     R    (3) If > 0 then do correction.
C     NUMROW       I    Number of rows in the table.
C     KLENG        I    Length of table record as kept in common.
C     KLOCD        I    Offset of Dec in table
C   Input/output in common:
C     MUTHA(huge)  R    Data array, See SDTRED for a description of the
C                       data ordering.  Weights set to 0 for flagged
C                       data.
C-----------------------------------------------------------------------
      INTEGER   LOOP, LIMIT, IP, JP, KP, BADCNT, NEXTOK(2000), LASTOK,
     *   OKCNT, START, NEXT
      REAL      DELTA, LOW, HIGH, DIFF, SAMDIF, TDEC(2), X
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       See if requested
      IF (BPARM(3).LE.1.0E-30) GO TO 999
      LIMIT = NUMROW - 1
      BADCNT = 0
C                                       Find average change
      IP = 1 + KLOCD
      DELTA = ABS ((MUTHA(IP+(NUMROW-1)*KLENG) - MUTHA(IP)) /
     *   (NUMROW - 1))
C                                       LOW = max. value for "low" diff.
      LOW = 0.8 * DELTA
C                                       HIGH = min. value for "high"
C                                       diff.
      HIGH = 1.5 * DELTA
C                                       NEXTOK is an array with one
C                                       value per sample.  If the value
C                                       is less than or equal 0 then the
C                                       associated declination is bad;
C                                       if it is positive then the
C                                       declination is good and the
C                                       value is the next higher sample
C                                       number with a good declination.
C                                       Loop through declinations
      IP = 1 + KLOCD
      JP = 1
      NEXTOK(1) = 0
      DO 100 LOOP = 1,LIMIT
         DIFF = ABS ((MUTHA(IP+KLENG)-MUTHA(IP)))
C                                       Low diff
         IF (DIFF.LE.LOW) THEN
            NEXTOK(JP+1) = 0
            BADCNT = 0
         ELSE IF (DIFF.GE.HIGH) THEN
C                                       High difference, second always
C                                       OK.
            OKCNT = OKCNT + 1
            BADCNT = 100
            NEXTOK(JP+1) = 1

C                                       Nominal difference - This is OK
C                                       iff not within 5 nomimals of a
C                                       low value
         ELSE
            BADCNT = BADCNT + 1
            IF (BADCNT.GT.5) THEN
C                                       OK
               OKCNT = OKCNT + 1
               NEXTOK(JP+1) = 1
            ELSE
               NEXTOK(JP+1) = 0
               END IF
            END IF
         IP = IP + KLENG
         JP = JP + 1
 100     CONTINUE
C                                       Need at least 5% good data
      IF (OKCNT.LE.0.05*NUMROW) THEN
         WRITE (MSGTXT,1100) OKCNT
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Loop backwards through
C                                       declinations finding next good
C                                       declination.
      JP = NUMROW
      LASTOK = NUMROW + 10
      DO 110 LOOP = 1,LIMIT
C                                       Good Dec?
         IF (NEXTOK(JP) .GT. 0) THEN
            NEXTOK(JP) = LASTOK
            LASTOK = JP
            END IF
         JP = JP - 1
 110     CONTINUE
C                                       Loop interpolating
      BADCNT = 0
      START = LASTOK + 1
      NEXT = NEXTOK(LASTOK)
      IP = 1 + KLOCD + (START-1) * KLENG
      DO 200 LOOP = START,NUMROW
         IF (NEXTOK(LOOP).LE.0) THEN
C                                       Bad - interpolate if later OK
C                                       point.
            IF (NEXT.GT.NUMROW) GO TO 210
            BADCNT = BADCNT + 1
            JP = 1 + KLOCD + ((LASTOK-1) * KLENG)
            KP = 1 + KLOCD + ((NEXT-1) * KLENG)
            TDEC(1) = MUTHA(JP)
            TDEC(2) = MUTHA(KP)
            SAMDIF = MAX ((NEXT - LASTOK), 1)
            X = LOOP - LASTOK
            MUTHA(IP) = TDEC(1) + ((TDEC(2) - TDEC(1)) * (X / SAMDIF))
         ELSE
C                                       Good - save info
            LASTOK = LOOP
            NEXT = NEXTOK(LOOP)
            END IF
         IP = IP + KLENG
 200     CONTINUE
 210     IF (BADCNT.GE.1) THEN
C                                       Tell if fixed Decs
            WRITE (MSGTXT,1200) BADCNT
            CALL MSGWRT (6)
            END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('TOO FEW GOOD DECLINATIONS (',I4,') FOUND')
 1200 FORMAT ('BADEC: Fixed ',I6,' bad declinations')
      END
      SUBROUTINE SDTTAB
C-----------------------------------------------------------------------
C   Routine to subtract tabulated baselines. Done if ARRAY2(1,1) or
C   ARRAY2(1,2) .ne. 0.  Does a Cubic spline interpolation in table
C   ARRAY2.   Declinations in ARRAY2 MUST enclose all values found.
C    Input from common:
C     ARRAY2(20,2) R    Baseline table,
C                       (i,1) = dec. i, ascending order.
C                       (i,2) = baseline i.
C     NUMROW       I    Number of rows in the table.
C     NUMCOL       I    Number of columns in table.
C     NUMRCX       I    Number of receivers
C     KPARM        I    Number of words of "random" data before data.
C     KLENG        I    Length of table record as kept in common.
C     KLOCD        I    Offset of Declination in table
C    Input/output in common:
C     MUTHA(huge)  R    Data array, See SDTRED for a description of the
C                       data ordering.
C     TBUFF(*)     R    Work arrays
C   Output in common:
C     RCXAVG(*)    R    Receiver average value, set to 0.0 if filtering
C                       done.
C-----------------------------------------------------------------------
      INTEGER   LOOP, IP, TABPNT(8000), TABNDX, TABINC, DATLOP, IPDAT
      REAL      DEC1, BASE(8000)
C                                       MAXN = max. NODE (no. points in
C                                       baseline table).
      INTEGER   MAXN, NODE
      PARAMETER (MAXN = 20)
      REAL      H, TNODE(MAXN), G(MAXN), WGS(MAXN), RHO, GSMO(MAXN),
     *   B(MAXN), C(MAXN), D(MAXN), TDEC
      INCLUDE 'SDTUV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TABPNT, BASE, TBUFF)
C-----------------------------------------------------------------------
C                                       See if requested
      IF ((ABS (ARRAY2(1,1)).LE.1.0E-30) .AND. (ABS
     *   (ARRAY2(1,2)).LE.1.0E-30)) GO TO 999
C                                       Check Dec order in MUTHA.
      TABINC = -1
      IF (MUTHA(1+KLOCD) .LT. MUTHA(1+KLOCD+KLENG)) TABINC = 1
C                                       Get Spline fit coefficients
      H = ARRAY2(2,1) - ARRAY2(1,1)
      RHO = 10.0
      DO 20 LOOP = 1,20
         IF (ABS (ARRAY2(LOOP,1)+ARRAY2(LOOP,2)) .GT. 0.0) NODE = LOOP
            TNODE(LOOP) = ARRAY2(LOOP,1)
            G(LOOP) = ARRAY2(LOOP,2)
            WGS(LOOP) = 1.0
 20      CONTINUE
      CALL DCSSMO (H, NODE, TNODE, G, WGS, RHO, GSMO, B, C, D)
C                                       Find first entry in table
      DEC1 = MUTHA(1+KLOCD)
      TABNDX = 0
      DO 50 LOOP = 1,20
         IF (ARRAY2(LOOP,1) .GT. DEC1) GO TO 60
         TABNDX = LOOP
 50      CONTINUE
C                                       BETTER not get here
      GO TO 900
 60   IF (TABNDX.LE.0) GO TO 900
C                                       TABNDX points to lower dec.
C                                       Get table pointers.
      IP = 1 + KLOCD
      DO 100 LOOP = 1,NUMROW
C                                       Check TABLE pointer
 70      IF ((MUTHA(IP).GT.ARRAY2(TABNDX,1)) .AND.
     *      (MUTHA(IP).LE.ARRAY2(TABNDX+1,1))) GO TO 80
            TABNDX = TABNDX + TABINC
            IF ((TABNDX.GT.20) .OR. (TABNDX.LE.0)) GO TO 900
            GO TO 70
 80      TABPNT(LOOP) = TABNDX
         IP = IP + KLENG
 100     CONTINUE
C                                       Compute baselines
      IP = 1 + KLOCD
      INCLUDE 'INCS:ZVND.INC'
      DO 200 LOOP = 1,NUMROW
         TABNDX = TABPNT(LOOP)
C                                       Use Spline fit
         TDEC = MUTHA(IP) - TNODE(TABNDX)
         BASE(LOOP) = GSMO(TABNDX) + ((D(TABNDX) * TDEC + C(TABNDX)) *
     *      TDEC + B(TABNDX)) * TDEC
         IP = IP + KLENG
 200     CONTINUE
C                                       Remove baseline
         IP = 1
         DO 300 LOOP = 1,NUMROW
            IPDAT = IP + KPARM
            DO 250 DATLOP = 1,NUMRCX
               MUTHA(IPDAT) = MUTHA(IPDAT) - (BASE(LOOP) +
     *            RCXAVG(DATLOP))
               MUTHA(IPDAT+1) = MUTHA(IPDAT+1) + (BASE(LOOP) +
     *            RCXAVG(DATLOP))
               IPDAT = IPDAT + 3
 250           CONTINUE
            IP = IP + KLENG
 300     CONTINUE
C                                       Zero receiver averages
      DO 600 LOOP = 1,NUMRCX
         RCXAVG(LOOP) = 0.0
 600     CONTINUE
      GO TO 999
C                                       Problem with table
 900  WRITE (MSGTXT,1100)
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('PROBLEM WITH TABLE; DOES NOT CONTAIN ALL DECS.')
      END
