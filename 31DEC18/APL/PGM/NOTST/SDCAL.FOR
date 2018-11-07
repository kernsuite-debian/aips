LOCAL INCLUDE 'SDCAL.INC'
C                                       Local include to SDCAL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, CNOIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSTOK, XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, XSTOK*4, NAMOUT*12, CLAOUT*6,
     *   HISCRD(10)*64
      REAL      XSIN, XDISIN, XTIME(8), XBIF, XEIF, XBCHAN, XECHAN,
     *   XSUBA, XDOCAL, XGUSE, XFLAG, XSOUT, XDISO, APARM(10),
     *   XBADD(10), BUFF1(UVBFSS)
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XTIME, XXSTOK, XBIF,
     *   XEIF, XBCHAN, XECHAN, XSUBA, XDOCAL, XGUSE, XFLAG, XNAMOU,
     *   XCLAOU, XSOUT, XDISO, APARM, XBADD,
     *   SEQIN, SEQOUT, DISKIN, DISKO, CNOIN, NUMHIS
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSTOK, NAMOUT, CLAOUT, HISCRD
LOCAL END
      PROGRAM SDCAL
C-----------------------------------------------------------------------
C! Applies a CS calibration table to a single dish data file.
C# Sdish Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2008, 2012, 2015
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
C   Applies a CS table to a single  dish data file.
C   Inputs:
C      AIPS adverb          Description.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      TIMERANG...Time range of the data to be copied.
C      STOKES.....Stokes type to pass.
C      BIF........First IF to copy. 0=>all.
C      EIF........Highest IF to copy. 0=>all higher than BIF
C      BCHAN......First channel to copy. 0=>all.
C      ECHAN......Highest channel to copy. 0=>all higher than BIF
C      SUBARRAY...Subarray number to copy. 0=>all.
C      DOCALIB....If true (>0) then calibrate the data.
C      GAINUSE....Version number of the Cal. table to use.
C      FLAGVER....Specifies the version of the flagging table.
C      OUTNAME....Output SD file name.
C      OUTCLASS...Output SD file name (class).    Standard defaults.
C      OUTSEQ.....Output UV file name (seq. #).   0 => highest unique
C      OUTDISK....Disk drive # of output UV file. 0 => highest with
C                 space for the file.
C      APARM......Control information:
C                    1 > 0 => avg. freq. in IF
C                    2 = Integration time (sec)
C                    3 > 0 => drop subarray info
C      BADDISK....Disks to avoid for scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SDCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'SDCAL '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL SDCTIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop over sources.
      CALL SDCTUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SDCTIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   SDCTIN gets input parameters for SDCAL, finds input file and
C   prepares the list of sources.  All selection criteria
C   except the source name is filled into the commons in D/CSEL.INC.
C   Inputs:  PRGN    C*6      Program name
C   Output:
C     JERR         I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in SDCAL for more details.
C-----------------------------------------------------------------------
      CHARACTER  STAT*4, PRGN*6, UTYPE*2
      INTEGER   JERR
      INTEGER   NPARM, IROUND, IERR, I
      REAL      CATR(256)
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SDCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR,CATBLK)
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 51
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
C                                       Crunch input parameters.
 20   SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Get CATBLK from old file.
 30   CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
 40   CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IERR
         GO TO 990
 50   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Save input header
      CALL RCOPY (256, CATBLK, CATUV)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       BADDISK
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 70      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      CALL RCOPY (8, XTIME, TIMRNG)
      STOKES = XSTOK
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (BIF.GT.EIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      DOCAL = XDOCAL.GT.0.0
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      DXTIME = APARM(2) / 86400.0
C                                       Get source list
      IUDISK = FVOL(1)
      IUCNO = FCNO(1)
      IXLUN = 28
      NSOUWD = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDCTIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 4000 FORMAT ('YOU ARE USING A NON-STANDARD PROGRAM')
      END
      SUBROUTINE SDCTUV (IRET)
C-----------------------------------------------------------------------
C   SDCTUV uses SDGET and SDCCOP to calibrate data.
C   The history and relevant tables are also copied.
C   Input:
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
      HOLLERITH CATH(256), CATUVH(256)
      LOGICAL   F, DOAVG, DOAPPT, FINISH, NOSUB, SMILE
      INTEGER   NUMVIS, TOTREC(2,3), IERR, ERRCNT, TMPVER, SUB, LIMS1,
     *   LIMS2, SUBTMP, CATSAV(256), OUTDSK, OUTCNO
      REAL      RPARM, VIS, CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION   FREQO(MAXIF)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'SDCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD),  (CATUV, CATUVH)
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      ERRCNT = 0
      SMILE = F
      TMPVER = CLVER
      DOAPPT = DOAPPL
      DOAVG = APARM(1).GT.0.0
      NOSUB = APARM(3).GT.0.0
C                                       Find number of subarrays
      IF (SUBARR.GT.1) THEN
         LIMS1 = SUBARR
         LIMS2 = SUBARR
      ELSE
         CALL FNDEXT ('AN', CATUV, LIMS2)
         LIMS1 = 1
         LIMS2 = MAX (1, LIMS2)
         END IF
      SUBTMP = SUBARR
C                                       Create header, fiddle tables etc
         SUBARR = SUBTMP
         CALL SDGET ('INIT', RPARM, VIS, IERR)
         DOAPPL = F
         CLVER = CLUSE
         IF (IERR.NE.0) GO TO 450
C                                       Close again - only need header
C                                       for sum of subarrays.
         CALL SDGET ('CLOS', RPARM, VIS, IERR)
         IF (IERR.NE.0) GO TO 450
C                                       Put new values in CATBLK.
         CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '    ', NAMOUT, CLAOUT,
     *      SEQOUT)
         CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CATBLK(KIIMS) = SEQOUT
C                                       Velocity etc. info IF=BIF
         CATD(KDRST) = 0.0
         CATD(KDARV) = 0.0
         CATR(KRARP) = 0.0
         CATBLK(KIALT) = 0
C                                       If averaging set CATBLK
         IF (DOAVG) THEN
            IF (BCHAN.EQ.0  .AND.  ECHAN.EQ.0 ) THEN
               CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF)
     *                           * CATBLK(KINAX+JLOCF)
            ELSE
               CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF)
     *                           * (ECHAN - BCHAN + 1)
               END IF
            CATBLK(KINAX+JLOCF) = 1
            END IF
C                                       Make sure there is data
         IF (CATBLK(KIGCN).LE.0) GO TO 450
C                                       Create output file.
         CCNO = 1
         CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
         IF (IERR.NE.2) GO TO 200
            WRITE (MSGTXT,1100)
            CALL MSGWRT (6)
 200     IF (IERR.NE.0) GO TO 450
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = CCNO
         FRW(NCFILE) = 2
         OUTDSK = DISKO
         OUTCNO = CCNO
C                                       copy keywords
         CALL KEYCOP (DISKIN, CNOIN, DISKO, CCNO, IERR)
C                                       Loop over subarrays.
         NUMVIS = 0
         DO 300 SUB = LIMS1,LIMS2
            SUBARR = SUB
C                                       Save CATBLK - SDGET will modify
            CALL COPY (256, CATBLK, CATSAV)
C                                       Initialize reading data
            CALL SDGET ('INIT', RPARM, VIS, IERR)
            IF (IERR.NE.0) GO TO 450
C                                       Update sort order if necessary
            IF (NUMVIS.GT.0) CALL CHR2H (2, '**', 1, CATSAV(KITYP))
C                                       Restore CATBLK
            CALL COPY (256, CATSAV, CATBLK)
C                                       Copy data
            FINISH = SUB.EQ.LIMS2
            CALL SDCCOP (NUMVIS, TOTREC, FINISH, DOAVG, DISKO, CCNO,
     *         NOSUB, FREQO, IERR)
            SMILE = SMILE .OR. (IERR.EQ.0)
            IF (IERR.NE.0) GO TO 450
            NUMVIS = NUMVIS + NVIS
 300        CONTINUE
C                                       History
         CLVER = TMPVER
         DOAPPL = DOAPPT
         SUBARR = SUBTMP
         CALL SDCTHI (OUTDSK, OUTCNO)
         CLVER = CLUSE
         DOAPPL = F
         NCFILE = NCFILE - 2
         GO TO 500
C                                       Error, close input file
C                                       then resume.
 450        CALL SDGET ('CLOS', RPARM, VIS, IERR)
            ERRCNT = ERRCNT + 1
C                                       Tell which source
            WRITE (MSGTXT,1450) SOURCS(1)
            CALL MSGWRT (6)
 500     CONTINUE
C                                       Make sure bad files
C                                       destroyed
      IRET = 1
      IF (SMILE) IRET = 0
      NCFILE = NCFILE - 1
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('CANNOT OVERWRITE OLD FILE')
 1450 FORMAT ('PROBLEM WITH SOURCE: ',A)
      END
      SUBROUTINE SDCTHI (DISK, CNO)
C-----------------------------------------------------------------------
C   SDCTHI copies and updates history file.  It also copies any tables
C   extension files.
C    Input:
C     DISK    I    Output file disk number
C     CNO     I    Output file catalog slot number.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(8)*2, HILINE*72, LABEL*8, UTYPE*2, STAT*4
      INTEGER   DISK, CNO
      INTEGER   IERR, I, LUN1, LUN2, NONOT
      REAL      BUFF2(1), CATR(256)
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SDCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (BUFF1(1025), BUFF2)
      EQUIVALENCE (CATBLK, CATR)
      DATA LUN1, LUN2 /28,29/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /8, 'CS','FG','NX','SU','SN','CC','CH','BP'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISK, CNOIN,
     *   CNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, CATBLK(KIIMS), DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Stokes'
      WRITE (HILINE,2010) TSKNAM, XSTOK
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IF range
      WRITE (HILINE,2001) TSKNAM, BIF, EIF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Chan range
      WRITE (HILINE,2002) TSKNAM, BCHAN, ECHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Subarray
      WRITE (HILINE,2003) TSKNAM, SUBARR
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Flagging
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,2004) TSKNAM, FGVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Calibration
      IF (DOCAL) THEN
C                                       Table
         WRITE (HILINE,2005) TSKNAM, CLUSE
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Averaging
         WRITE (HILINE,2011) TSKNAM, APARM(1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Integration time
         WRITE (HILINE,2012) TSKNAM, APARM(2)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                      Add any user supplied history.
100   IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1010) TSKNAM
         DO 150 I = 1,NUMHIS
            HILINE = LABEL // HISCRD (I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 150        CONTINUE
      NUMHIS = 0
C                                       Close HI file
 200   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN,
     *   DISK,  CNOIN, CNO, CATBLK, BUFF1, BUFF2, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clear status
      UTYPE = 'UV'
      STAT = 'CLWR'
      CALL CATDIR ('CSTA', DISK, CNO, NAMOUT, CLAOUT, CATBLK(KIIMS),
     *   UTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDCTHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 2001 FORMAT (A6,' BIF =',I4,', EIF =',I4,'/ IF RANGE')
 2002 FORMAT (A6,' BCHAN =',I4,', ECHAN =',I4,'/ CHAN RANGE')
 2003 FORMAT (A6,' SUBARRAY =',I4)
 2004 FORMAT (A6,' FGVER = ',I3, '/ Edited using FG table')
 2005 FORMAT (A6,' GAINUSE =',I3,' / CS table')
 2010 FORMAT (A6,' STOKES = ''',A4,''' / STOKES type')
 2011 FORMAT (A6,' APARM(1) =',F5.2,' / >0 => Average in freq.')
 2012 FORMAT (A6,' APARM(2) =',F5.2,' / Integration time (sec)')
      END
      SUBROUTINE SDCCOP (VISOFF, TOTREC, FINISH, DOAVG, DISK, CNO,
     *   NOSUB, FREQO, IRET)
C-----------------------------------------------------------------------
C   SDCCOP reads and corrects data files with optional averaging
C   over the frequency axis.
C   Input:
C    VISOFF      I    Offset in output file
C    TOTREC(2,3) I    Total counts of record flagging
C    FINISH      L    If True, this finishes the source, compress, etc.
C    DOAVG       L    If true, average frequencies in IF
C    DISK        I    Output disk number.
C    CNO         I    Output catalog slot number.
C    NOSUB       L    IF True drop subarray code.
C    FREQO(*)    D    IF Frequency offsets for source (Hz)
C   Input from common:
C    JBUFSZ      I    Size of BUFF1
C    INCF   I     Increment in freq. of data from SDGET
C    INCIF  I     Increment in IF of data from SDGET
C    INCS   I     Increment in Stokes' of data from SDGET
C    JLOCF  I     Offset of freq. of data from SDGET
C    JLOCIF I     Offset of IF of data from SDGET
C    JLOCS  I     Offset of Stokes' of data from SDGET
C   Output:
C    BUFF1  R     Output I/O buffer.
C    IRET   I     Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      CHARACTER NAME*48
      INTEGER   VISOFF, DISK, CNO, IRET
      LOGICAL   FINISH, DOAVG, NOSUB
      DOUBLE PRECISION    FREQO(*)
      INTEGER   LUN, FIND, BIND, LENBU, NIO, LOOPS, LOOPIF, LOOPF,
     *   JNCIF, JNCS, LRECO, INP, NUMFRQ, NNIF, NOPOL
      LOGICAL   T, F
      INTEGER   BO, I, XCOUNT, INDEX, OUTDEX, IIVER, TOTREC(2,3), BLCODE
      REAL SUMWT, SUMRE, SUMIM, XNORM, WT, VIS(4098)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SDCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
C
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      DOUBLE PRECISION FOFF(MAXIF)
C
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, BO /18,1/
C-----------------------------------------------------------------------
C                                       Set lengths of input axes.
      NUMFRQ = ECHAN - BCHAN + 1
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      NOPOL = CATBLK(KINAX+JLOCS)
C                                       Set output increments
C                                       (averaging)
      JNCIF = INCIF
      IF (JLOCF.LT.JLOCIF) JNCIF = INCIF / NUMFRQ
      JNCS = INCS
      IF (JLOCF.LT.JLOCS) JNCS = INCS / NUMFRQ
C                                       If output file already open
C                                       close it.
      IF ((FTAB(FIND).EQ.LUN) .AND. (VISOFF.LE.0))
     *   CALL ZCLOSE (LUN, FIND, IRET)
C                                       Setup for new file
      IF (VISOFF.GT.0) GO TO 30
C                                       Zero flag counts
         TOTREC(1,1) = 0
         TOTREC(2,1) = 0
         TOTREC(1,2) = 0
         TOTREC(2,2) = 0
         TOTREC(1,3) = 0
         TOTREC(2,3) = 0
C                                       Set output file name.
         CALL ZPHFIL ('UV', DISK, CNO, 1, NAME, IRET)
C                                       Open output file.
         CALL ZOPEN (LUN, FIND, DISK, NAME, T, F, T, IRET)
         IF (IRET.EQ.0) GO TO 20
            WRITE (MSGTXT,1000) IRET
            GO TO 990
C                                       Init vis file for write
 20      LENBU = 1
         LRECO = LREC
         IF (DOAVG) LRECO = NRPARM + (LREC-NRPARM) / NUMFRQ
         CALL UVINIT ('WRIT', LUN, FIND, CATBLK(KIGCN), VISOFF, LRECO,
     *      LENBU, JBUFSZ, BUFF1, BO, BIND, IRET)
         IF (IRET.EQ.0) GO TO 30
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       Copy file
 30   XCOUNT = 0
      DO 100 I = 1,NVIS
         IF (DOAVG) GO TO 40
C                                       No averaging:
            CALL SDGET ('READ', BUFF1(BIND), BUFF1(BIND+NRPARM), IRET)
            IF (IRET.LT.0) GO TO 110
            IF (IRET.NE.0) GO TO 999
            GO TO 90
C                                       Average in freq in each IF
 40         CALL SDGET ('READ', BUFF1(BIND), VIS, IRET)
            IF (IRET.LT.0) GO TO 110
            IF (IRET.NE.0) GO TO 999
C                                       Average.
            DO 70 LOOPS = 1,NOPOL
               DO 70 LOOPIF = 1,NUMIF
                  SUMWT = 0.0
                  SUMRE = 0.0
                  SUMIM = 0.0
                  INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-1) * INCIF
                  OUTDEX = BIND + NRPARM + (LOOPS-1) * JNCS +
     *               (LOOPIF-1) * JNCIF
                  DO 50 LOOPF = 1,NUMFRQ
                     INP = INDEX + (LOOPF-1) * INCF
                     WT = VIS(INP+2)
                     IF (WT.LE.0.0) WT = 0.0
                     SUMRE = SUMRE + VIS(INP) * WT
                     SUMIM = SUMIM + VIS(INP+1) * WT
                     SUMWT = SUMWT + WT
 50                  CONTINUE
                  XNORM = 1.0
                  IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
                  BUFF1(OUTDEX) = SUMRE * XNORM
                  BUFF1(OUTDEX+1) = SUMIM * XNORM
                  BUFF1(OUTDEX+2) = SUMWT
 70               CONTINUE
C                                       Drop subarray if requested
 90      IF (ILOCB.GE.0) THEN
            BLCODE = BUFF1(BIND+ILOCB)
            IF (NOSUB) BUFF1(BIND+ILOCB) = BLCODE
         ELSE IF (NOSUB) THEN
            BUFF1(BIND+ILOCSA) = 1.0
            END IF
C                                       Write new
         NIO = 1
         XCOUNT = XCOUNT + 1
         CALL UVDISK ('WRIT', LUN, FIND, BUFF1, NIO, BIND, IRET)
         IF (IRET.EQ.0) GO TO 100
            WRITE (MSGTXT,1090) IRET
            GO TO 990
 100        CONTINUE
C                                       If FINISH shut down output
 110  NVIS = XCOUNT
C                                       Sum flag counts
         TOTREC(1,1) = TOTREC(1,1) + CNTREC(1,1)
         TOTREC(2,1) = TOTREC(2,1) + CNTREC(2,1)
         TOTREC(1,2) = TOTREC(1,2) + CNTREC(1,2)
         TOTREC(2,2) = TOTREC(2,2) + CNTREC(2,2)
         TOTREC(1,3) = TOTREC(1,3) + CNTREC(1,3)
         TOTREC(2,3) = TOTREC(2,3) + CNTREC(2,3)
      IF (.NOT.FINISH) GO TO 800
C                                       Flush output
         NIO = 0
         CALL UVDISK ('FLSH', LUN, FIND, BUFF1, NIO, BIND, IRET)
         IF (IRET.EQ.0) GO TO 120
            WRITE (MSGTXT,1090) IRET
            GO TO 990
C                                       Compress output file.
 120     NVIS = XCOUNT + VISOFF
         CALL UCMPRS (NVIS, DISK, CNO, LUN, CATBLK, IRET)
C                                       Update CATBLK.
         CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFF1, IRET)
         IF (IRET.EQ.0) GO TO 140
            WRITE (MSGTXT,1130) IRET
C                                       Copy relevant portion of IF
C                                       table.
 140     IF (JLOCIF.GT.0) THEN
C                                       Read old
            IIVER = 1
            CALL CHNDAT ('READ', BUFF1, IUDISK, IUCNO, IIVER, CATUV,
     *         LUN, NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Fixup
            NNIF = EIF - BIF + 1
            DO 150 I = BIF,EIF
               FOFF(I) = FOFF(I) + FREQO(I)
 150           CONTINUE
C                                       Rewrite new
            IIVER = 1
            CALL CHNDAT ('WRIT', BUFF1, DISK, CNO, IIVER, CATBLK, LUN,
     *         NNIF, FOFF(BIF), ISBAND(BIF), FINC(BIF), BNDCOD(BIF),
     *         FRQSEL, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Give data summary
         IF (DOCAL) THEN
            WRITE (MSGTXT,2800)
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),2800)
            NUMHIS = NUMHIS + 1
            WRITE (MSGTXT,2801) TOTREC(1,1), TOTREC(1,2), TOTREC(1,3)
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),2801) TOTREC(1,1), TOTREC(1,2),
     *         TOTREC(1,3)
            NUMHIS = NUMHIS + 1
            WRITE (MSGTXT,2802) TOTREC(2,1), TOTREC(2,2), TOTREC(2,3)
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),2802) TOTREC(2,1), TOTREC(2,2),
     *         TOTREC(2,3)
            NUMHIS = NUMHIS + 1
         ELSE
            WRITE (MSGTXT,2803) NVIS
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),2803) NVIS
            NUMHIS = NUMHIS + 1
            END IF
         IF (NVIS.LE.0) THEN
C                                       No data found.
            IRET = 9
            WRITE (MSGTXT,1800)
            GO TO 990
            END IF
C                                       Close files
 700     CALL ZCLOSE (LUN, FIND, IRET)
 800  CALL SDGET ('CLOS', BUFF1(BIND), BUFF1(BIND+NRPARM), IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDCCOP: ERROR',I5,' OPENING OUTPUT FILE')
 1020 FORMAT ('SDCCOP: ERROR',I5,' INIT. OUTPUT FILE')
 1090 FORMAT ('SDCCOP: ERROR',I5,' WRITING OUTPUT FILE')
 1120 FORMAT ('SDCCOP: ERROR',I3,' COMPRESSING UV FILE',1X,F4.1,
     *   ' TIMES TOO BIG')
 1130 FORMAT ('SDCCOP: ERROR',I3,' UPDATING CATALOGUE HEADER')
 1800 FORMAT ('SDCCOP: ERROR - NO DATA WRITTEN')
 2800 FORMAT (10X,' Previously flagged ','  flagged by gain   ',
     *   '      kept')
 2801 FORMAT ('Partially ',2(I15,5X),I10)
 2802 FORMAT ('Fully     ',2(I15,5X),I10)
 2803 FORMAT (I9,' visibilities written')
      END
