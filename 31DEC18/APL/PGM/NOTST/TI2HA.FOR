LOCAL INCLUDE 'TI2HA.INC'
C                                       Local include for TI2HA
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XXSTOK,
     *   XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XSOUT, XDISO,
     *   DOARAY, XCENT, XREFA, BADD(10),  BUFF2(UVBFSS), DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT, OLDCNO,
     *   CATNEW(256), LRECI, LRECO, NRPRMI, NRPRMO, NEWCNO, REFANT
      LOGICAL   ISCOMP, ISHA
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN,
     *   XECHAN, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, XNAMOU, XCLAOU, XSOUT, XDISO, DOARAY, XCENT,
     *   XREFA, BADD
      COMMON /PRMS/ CATNEW, BUFF2, JBUFSZ, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, LRECI, LRECO, NRPRMI, NRPRMO, ISCOMP, OLDCNO, NEWCNO,
     *   ISHA, DIFPIX, REFANT
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
C                                       End local include for TI2HA
LOCAL END
      PROGRAM TI2HA
C-----------------------------------------------------------------------
C! Substitutes the hour angle for the IAT
C# UV UV-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1998-2001, 2005-2010, 2012-2015, 2018
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
C   TI2HA substitutes the hour angle for the IAT in single-source uv
C   data sets.  A FUDGE clone.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'TI2HA.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'TI2HA '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL TIHAIN (PRGM, IRET)
C                                       Copy data fixing time
      IF (IRET.EQ.0) CALL TIHAUV (IRET)
C                                       HI and Table files
      IF (IRET.EQ.0) CALL TIHAHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF2)
C
 999  STOP
      END
      SUBROUTINE TIHAIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   TIHAIN gets input parameters for TI2HA and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      ISCOMP  L  If true data are compressed
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2, KEYWRD*8, KEYVAL*8
      HOLLERITH CATH(256), VALUES(2)
      INTEGER   IROUND, NPARM, IERR, I, NFREQ, LUN, LOCS, KEYTYP, INCX
      LOGICAL   MATCH
      REAL      RPARM(20), CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'TI2HA.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK, KEYWRD /' ', 'TIMETYPE'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 177
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF2, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      REFANT = IROUND (XREFA)
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
C                                       Set time range.
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1031) IERR
         GO TO 990
         END IF
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       already in hour angle
      CALL CATKEY ('REED', DISKIN, OLDCNO, KEYWRD, 1, LOCS, VALUES,
     *   KEYTYP, BUFF2, IERR)
      IF (IERR.EQ.0) THEN
         CALL H2CHR (8, 1, VALUES, KEYVAL)
         ISHA = KEYVAL.EQ.'HourAngl'
      ELSE
         ISHA = .FALSE.
         END IF
      IF (ISHA) THEN
         MSGTXT = '****** ALREADY IN HOUR ANGLE, COPY ONLY ******'
         CALL MSGWRT (7)
         END IF
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         BIF = MIN (MAX (1, BIF), CATBLK(KINAX+JLOCIF))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
      NFREQ = CATBLK(KINAX+JLOCF)
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      IF ((BCHAN.LE.0) .OR. (BCHAN.GT.NFREQ)) BCHAN = 1
      IF ((ECHAN.LE.0) .OR. (ECHAN.GT.NFREQ)) ECHAN = NFREQ
      IF (BCHAN.GT.ECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         JERR = 1
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF2, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF2, IERR)
C                                       multi-source ?
      IF (ILOCSU.GE.0) THEN
         MSGTXT = 'YOU MUST SELECT ONLY ONE SOURCE'
         IERR = 8
         GO TO 990
         END IF
C                                       Save input file info
      LRECI = LREC
      NRPRMI = NRPARM
C                                       center frequencies?
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.GT.0.0) THEN
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
      ELSE
         DIFPIX = 0.0
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       read compressed => write compr.
      IF (ISCOMP) THEN
         CATBLK(KINAX) = 1
         I = CATBLK(KIPCN)
         CALL CHR2H (8, 'WEIGHT  ', 1, CATBLK(KHPTP+2*I))
         CALL CHR2H (8, 'SCALE   ', 1, CATBLK(KHPTP+2*I+2))
         CATBLK(KIPCN) = I + 2
         ILOCWT = I
         END IF
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            MSGTXT = 'MAY OVERWRITE INPUT FILE ONLY.  QUITTING'
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
      NEWCNO = CCNO
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
C                                       put special keyword
      LOCS = 1
      KEYTYP = 3
      CALL CHR2H (8, 'HourAngl', 1, VALUES)
      CALL CATKEY ('WRIT', DISKO, CCNO, KEYWRD, 1, LOCS, VALUES,
     *   KEYTYP, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'Error writing TIMETYPE header keyword'
         CALL MSGWRT (6)
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATNEW)
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      LRECO = LREC
      NRPRMO = NRPARM
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF2, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TIHAIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1031 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1065 FORMAT ('TIHAIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE TIHAUV (IRET)
C-----------------------------------------------------------------------
C   TIHAUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      ISCOMP  L  If true data are compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRI, IPTRO, LUNO, INDO, ILENBU, LUNT, KBIND, NIOUT,
     *   NIOLIM, IA1, IA2, BO, VO, NUMVIS, XCOUNT, NCORI, NCORO, JANT,
     *   NCOPY, JSUB, ISUB, NSUB, TABUFF(512), NSOU, ISUB1, ISUB2
      LOGICAL   T, F, ORDERD
      INCLUDE 'TI2HA.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      REAL     BASEN, HANGLE, LTIME, TIME, RPARM(20), VIS(UVBFSS)
      DOUBLE PRECISION UVSCAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA LUNO, LUNT /17, 18/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       subarray loop
      ORDERD = .TRUE.
      CALL FNDEXT ('AN', CATUV, NSUB)
      NSUB = MAX (1, NSUB)
      JSUB = XSUBA + 0.1
      IF ((JSUB.GE.1) .AND. (JSUB.LE.NSUB)) THEN
         ISUB1 = JSUB
         ISUB2 = JSUB
      ELSE
         ISUB1 = 1
         ISUB2 = NSUB
         END IF
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CATUV(KINAX)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, NEWCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       get header source info: no error
      NSOU = 0
      IDSOUR = 0
      CALL GETSOU (NSOU, DISKIN, OLDCNO, CATNEW, LUNT, IRET)
      NUMVIS = 0
      XCOUNT = 0
      LTIME = -100.
C                                       Init vis file for read.
      DO 200 SUBARR = ISUB1,ISUB2
         CALL GETANT (DISKIN, OLDCNO, SUBARR, CATUV, TABUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1025) IRET, ISUB
            CALL MSGWRT (7)
            GO TO 200
            END IF
         ILENBU = 0
         JANT = REFANT
C                                       Open and init for read
         CALL UVGET ('INIT', RPARM, VIS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET
            GO TO 990
            END IF
         CALL COPY (256, CATNEW, CATBLK)
         CALL UVPGET (IRET)
         IF ((FREQ.GT.0.0D0) .AND. (UVFREQ.GT.0.0D0)) THEN
            UVSCAL = FREQ / UVFREQ
         ELSE
            UVSCAL = 1.0D0
            END IF
C                                       Loop
C                                       Read vis. record.
 100     CALL UVGET ('READ', RPARM, VIS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
C                                       Loop over buffer
         ELSE IF (IRET.EQ.0) THEN
            IF (ILOCB.GE.0) THEN
               BASEN = RPARM(1+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
               JSUB = 100.0 * (BASEN - IA2 - 256*IA1) + 1.5
            ELSE
               IA1 = RPARM(1+ILOCA1) + 0.1
               IA2 = RPARM(1+ILOCA2) + 0.1
               JSUB = RPARM(1+ILOCSA) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
            IF (.NOT.ISHA) THEN
               TIME = RPARM(1+ILOCT)
               IF ((TIME.GT.5*(SUBARR-1)) .AND. (DOARAY.GT.0.0)) THEN
                  TIME = TIME - 5*(SUBARR-1)
                  IF (ILOCB.GE.0) THEN
                     RPARM(IPTRI+ILOCB) = RPARM(IPTRI+ILOCB) -
     *                  0.01 * (SUBARR-1)
                  ELSE
                     RPARM(1+ILOCSA) = 1.0
                     END IF
                  END IF
               IF (REFANT.LE.0) JANT = IA1
               CALL HOURAN (JANT, TIME, HANGLE)
               RPARM(1+ILOCT) = HANGLE + 1.0
               END IF
            IF (RPARM(1+ILOCT).LT.LTIME) ORDERD = .FALSE.
            LTIME = RPARM(1+ILOCT)
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, VIS, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, VIS, BUFF2(IPTRO+NRPRMO))
               END IF
            XCOUNT = XCOUNT + 1.0D0
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
            GO TO 100
            END IF
C                                       close this subarray
         CALL UVGET ('CLOS', RPARM, VIS, IRET)
 200     CONTINUE
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, NEWCNO, LUNO, CATBLK, IRET)
      IF (.NOT.ORDERD) CALL CHR2H (2, '  ', 1, CATBLK(KITYP))
C                                       Close files
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TIHAUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('TIHAUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('TIHAUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1025 FORMAT ('TIHAUV: ERROR',I3,' READING AN FILE',I3,' SKIPPING')
 1100 FORMAT ('TIHAUV: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('TIHAUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE TIHAHI
C-----------------------------------------------------------------------
C   TIHAHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DSEL.INC'
      CHARACTER NOTTYP(16)*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, NONOT, BUFF1(512), I, ISURNO, SIDSOU,
     *   SQUAL, SUFQID, NSOURC, INOGRP, KOLS(MAXSUC), NUMV(MAXSUC), VER,
     *   NNIF, FREQID
      LOGICAL   ISBAND(MAXIF)
      CHARACTER VELTYP*8, VELDEF*8, SSNAME*16, SCALCO*4, BNDCOD(MAXIF)*8
      DOUBLE PRECISION SBANDW, SRAEPO, SDECEP, SEPOCH, SRAAPP, SDECAP,
     *   SPMRA, SPMDEC, SLSRVE(MAXIF), SFREQO(MAXIF), SLREST(MAXIF),
     *   FOFF(MAXIF), FQOFF, CATD(128), SRAOBS, SDECOB
      REAL     SFLUX(4,MAXIF), FINC(MAXIF), CATR(256)
      INCLUDE 'TI2HA.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA LUN1, LUN2 /27,28/
      DATA NONOT, NOTTYP /16, 'NX','FQ','CH','CL','SN','SU','FG','BP',
     *   'IM', 'CQ', 'PC', 'TY', 'GC', 'MC', 'WX', 'AN'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       is hour angle already
      IF (ISHA) THEN
         HILINE = TSKNAM // '/ times already in hour angle - ' //
     *      'did simple copy'
      ELSE IF (REFANT.GT.0) THEN
         WRITE (HILINE,1010) TSKNAM, REFANT
      ELSE
         HILINE = TSKNAM // '/ hour angle refernce to lower ant #' //
     *      ' each vis'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
C                                       Close HI file
 200  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                        Copy tables
      IF (DOARAY.LE.0.0) NONOT = NONOT - 1
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'TIHAHI: ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, NEWCNO, DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'TIHAHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                       copy antenna file 1 if DOARAY
      IF (DOARAY.GT.0.0) THEN
         CALL TABCOP ('AN', 1, 1, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *      NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            MSGTXT = 'TIHAHI: ERROR COPYING SINGLE AN TABLE'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       FQ table
      IF (JLOCIF.GT.0) THEN
         CALL DFILL (MAXIF, 0.0D0, SFREQO)
C                                       Multi to single source
         IF ((KLOCSU.GE.0) .AND. (ILOCSU.LT.0)) THEN
C                                       Open file
            CALL SOUINI ('READ', BUFF2, IUDISK, IUCNO, 1, CATUV, LUN1,
     *         INOGRP, VELTYP, VELDEF, SUFQID, I, KOLS, NUMV, IERR)
            IF (IERR.NE.0) GO TO 220
C                                       Get number of sources.
            NSOURC = BUFF2(5)
C                                       Loop looking for source.
            DO 210 I = 1,NSOURC
               ISURNO = I
               CALL TABSOU ('READ', BUFF2, ISURNO, KOLS, NUMV, SIDSOU,
     *            SSNAME, SQUAL, SCALCO, SFLUX, SFREQO, SBANDW, SRAEPO,
     *            SDECEP, SEPOCH, SRAAPP, SDECAP, SRAOBS, SDECOB,
     *            SLSRVE, SLREST, SPMRA, SPMDEC, IERR)
               IF (IERR.GT.0) CALL DFILL (MAXIF, 0.0D0, SFREQO)
               IF ((SIDSOU.EQ.SOUWAN(1)) .OR. (IERR.GT.0)) GO TO 215
 210           CONTINUE
C                                       Didn't find
            CALL DFILL (MAXIF, 0.0D0, SFREQO)
C                                       Close file
 215        CALL TABIO ('CLOS', 0, I, BUFF2, BUFF2, IERR)
            END IF
C                                       Read old
 220     VER = 1
         CALL CHNDAT ('READ', BUFF1, DISKIN, OLDCNO, VER, CATUV, LUN1,
     *      NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Fixup
         NNIF = EIF - BIF + 1
         FQOFF = FOFF(BIF)
C                                       force the first IF to zero
         DO 230 I = BIF,EIF
            FOFF(I) = FOFF(I) - FQOFF + (SFREQO(I) - SFREQO(BIF))
 230        CONTINUE
C                                       Output ref IF = 1
         CATD(KDCRV+JLOCIF) = 1.0D0
         CATR(KRCRP+JLOCIF) = 1.0
C                                       Rewrite new
         VER = 1
         FREQID = 1
         CALL CHNDAT ('WRIT', BUFF1, DISKO, NEWCNO, VER, CATBLK, LUN1,
     *      NNIF, FOFF(BIF), ISBAND(BIF), FINC(BIF), BNDCOD(BIF),
     *      FREQID, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (MSGTXT,1230) DISKIN, OLDCNO, VER, DISKO, NEWCNO, VER
         CALL MSGWRT (3)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TIHAHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A,'REFANT =',I4,5X,'/ hour angle reference antenna')
 1230 FORMAT ('Copied FQ file from vol/cno/vers',I3,I5,I4,' to',
     *   I3,I5,I4)
      END
      SUBROUTINE HOURAN (IANT, TIME, HANGLE)
C-----------------------------------------------------------------------
C   Subroutine to compute the hour angle at a given time of the antennas
C   in the antenna common.  All antennas of the VLA have the same hour
C   angle, but the concept of hour angle is less meaningful with VLB
C   observations.
C   Inputs:
C      IANT       I      Find hour angle wrt this antenna
C      TIME       R      Current data time (days).
C   Input from common:
C      RAAPP      D      Apparent RA of source
C      DECAPP     D      Apparent Declination of source.
C      MNTYP(*)   I      Mount type 0=> atl-az.
C      STNLAT(*)  D      Antenna latitude (rad).
C      STNLON(*)  D      Antenna east longitudes (rad).
C      GSTIAT     D      GST at IAT=0 of reference day (rad).
C      ROTIAT     D      Rotation of the earth rate in IAT.
C   Output:
C      HANGLE     R      Hour angle for antenna 1 in days
C-----------------------------------------------------------------------
      INTEGER   IANT
      REAL      TIME, HANGLE
C
      INTEGER   I
      LOGICAL   ISVLA
      DOUBLE PRECISION HRANG, ANTLST, ARLONG
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Is this the VLA?
      ISVLA = (ABS (CNTRX + 1.601162D6) .LE. 10.0D0) .AND.
     *   (ABS (CNTRY + 5.042003D6) .LE. 10.0D0) .AND.
     *   (ABS (CNTRZ - 3.554915D6) .LE. 10.0D0)
      ISVLA = ISVLA .OR. ((ABS (CNTRX + 1.601185D6) .LE. 10.0D0) .AND.
     *   (ABS (CNTRY + 5.041978D6) .LE. 10.0D0) .AND.
     *   (ABS (CNTRZ - 3.554875D6) .LE. 10.0D0))
C                                       All VLA antennas have the same
C                                       parallactic angle.
      IF (ISVLA) THEN
         ARLONG = ATAN2 (CNTRY, CNTRX)
      ELSE
         ARLONG = STNLON(IANT)
         END IF
C                                       Antenna LST
      ANTLST = GSTIAT + ARLONG + TIME * ROTIAT
C                                       Hour angle
      HRANG = (ANTLST - RAAPP) / TWOPI
      I = HRANG
      HRANG = HRANG - I
      IF (HRANG.GT.0.5D0) HRANG = HRANG - 1.0D0
      IF (HRANG.LT.-0.5D0) HRANG = HRANG + 1.0D0
      HANGLE = HRANG
C
 999  RETURN
      END
