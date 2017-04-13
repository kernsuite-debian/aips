LOCAL INCLUDE 'CORER.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JBUFSZ, IUSER, SEQIN, DISKIN, LIMIT2, ANTMAX, CNOIN,
     *   SCBUFF(256), FGVERO, FGVERI, NFGWRI, NPOL, NCHAN, NIF, DISKOU,
     *   CNOOUT, NCFLAG, SEQOUT, CATOLD(256), CATNEW(256), ILOCWT,
     *   NCDEL
      REAL      BUFF1(UVBFSS), BUFF2(UVBFSS), PTFLUX(4), TIME1, TIME2,
     *   FLUX(4), RMSL(4)
      CHARACTER ISTOKE(4)*2
      LOGICAL   WASBAD, FSTOKE(4,4), REREAD, NEEDRL, ISCOMP
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), UVRANG(2),
     *   XFGOUT, DOOUT, XOUTS, XOUTD, DOCRT, PRTLEV, CPARM(10)
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XOUTN(3),
     *   XOUTC(2), XLPNAM(12)
      CHARACTER NAMEIN*12, CLAIN*6, LPNAME*48, NAMOUT*12, CLAOUT*6,
     *   REASON*24
      INTEGER   LFGRNO, FGLUN, FGKOLS(MAXFGC), FGNUMV(MAXFGC), FGSID,
     *   FGBUFF(512), FGSUBA, FGFQID, FGANT1, FGANT2, FGBIF, FGEIF,
     *   FGBCH, FGECH, LBCHAN, LBIF
      REAL      FGTS, FGTE
      COMMON /BUFRS/ BUFF1, BUFF2, SCBUFF, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN, XECHAN,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH, UVRANG, XFGOUT, DOOUT, XOUTN, XOUTC, XOUTS, XOUTD,
     *   DOCRT, XLPNAM, PRTLEV, CPARM
      COMMON /TSPARM/ CATOLD, CATNEW, PTFLUX, TIME1, TIME2, FLUX, RMSL,
     *   SEQIN, DISKIN, LIMIT2, ANTMAX, FGVERI, FGVERO, NFGWRI, CNOIN,
     *   NPOL, NCHAN, NIF, WASBAD, FSTOKE, DISKOU, CNOOUT, REREAD,
     *   IUSER, NEEDRL, NCFLAG, SEQOUT, ISCOMP, ILOCWT, NCDEL
      COMMON /CHARPM/ NAMEIN, CLAIN, LPNAME, ISTOKE, NAMOUT, CLAOUT,
     *   REASON
      COMMON /FGINFO/ FGBUFF, FGLUN, LFGRNO, FGKOLS, FGNUMV, FGSID,
     *   FGSUBA, FGFQID, FGANT1, FGANT2, FGTS, FGTE, FGBIF, FGEIF,
     *   FGBCH, FGECH, LBCHAN, LBIF
LOCAL END
      PROGRAM CORER
C-----------------------------------------------------------------------
C! Calculates average and rms of visibilities for each correlator
C# Spectral Utility UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2004, 2007-2008, 2010, 2012, 2015-2016
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
C   CORER is an AIPS System task to read through a data base and
C   calculate the average and rms value for each correlator.  This
C   task is mainly used to look for correlator offsets.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      USERID         IUSER         UV data owner number
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      DOCRT          DOCRT         > 0 => use CRT, else printer
C      OUTPRINT       LPNAME        File to keep printer output in
C      UVRANGE(2)     UVRANG        Range of search in kilolambda
C      TIMERANG       TIME1         Start time: TIMER(1 - 4) =
C                                      dd, hh, mm, ss
C                     TIME2         Stop time: TIMER(5 - 8) =
C                                      dd, hh, mm, ss
C      CPARM(1,2)     FLUX          Print and flag only pts >
C                                   FLUX * expected error in mean
C                                   1 for IPOL, 2 for cross POL
C      CPARM(3,4)     RMSL          Print and delete all correlators
C                                   with rms > RMSL.   0 -> 1.E9
C                                   3 for IPOL, 4 for cross POL
C      CPARM(5)       PTFLUX        Flux to subtract as unpolarized
C                                   point source at origin
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   NC, NANT, IRET, NWORDS, IERR, NW
      LONGINT   OFFSET
      REAL      SUMS(2)
      INCLUDE 'CORER.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'CORER '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL CRERIN (PRGM, NANT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       dynamic memory for summing
      NC = NPOL * NCHAN * NIF
      NW = NC * NANT * NANT
      NWORDS = 5 * NW
      NWORDS = (NWORDS - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SUMS, OFFSET, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) NWORDS
         CALL MSGWRT (8)
C                                       Sum and print data
      ELSE
         CALL CRERUV (NC, NANT, SUMS(1+OFFSET), SUMS(1+OFFSET+NW),
     *      SUMS(1+OFFSET+2*NW), SUMS(1+OFFSET+3*NW),
     *      SUMS(1+OFFSET+4*NW), IRET)
         IF ((IRET.EQ.0) .AND. (REREAD)) CALL CRERRE (NC, NANT,
     *      SUMS(1+OFFSET), IRET)
         IF (IRET.EQ.0) CALL CRERHI
         CALL ZMEMRY ('FREE', TSKNAM, NWORDS, SUMS, OFFSET, IERR)
         END IF
C                                       Close down
 990  CALL DIE (IRET, SCBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ATTEMPT TO ALLOCATE',I12,' WORDS MEMORY FAILS')
      END
      SUBROUTINE CRERIN (PRGM, NANT, IRET)
C-----------------------------------------------------------------------
C   CRERIN gets input parameters for CORER.
C   Inputs:
C      PRGM   C*6   Program name
C   Output:
C      NANT   I     Maximum antenna number
C      IRET   I     0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   NANT, IRET
C
      CHARACTER STAT*4, UTYPE*2, JSTOKE(13)*2, BLANK*4, CTIME*8,
     *   CDATE*12
      INTEGER   I, IERR, JJ, J, NPARM, IROUND, LUN, NVER, NUMAN(513),
     *   IDATE(3), ITIME(3)
      REAL      RPARM(20), TEMP, CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      LOGICAL   MATCH
      INCLUDE 'CORER.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATH, CATD, CATBLK)
      DATA BLANK /' '/
      DATA JSTOKE /'YX','XY','YY','XX', 'LR','RL','LL','RR', '??',
     *   'I ','Q ','U ','V '/
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 191
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, SCBUFF, IRET)
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IRET
            CALL MSGWRT (8)
         END IF
      IF ((NPOPS.GT.NINTRN) .OR. (IRET.NE.0) .OR. (ISBTCH.EQ.32000))
     *   THEN
         IF (DOCRT.NE.0.0) DOCRT = MIN (-1.0, DOCRT)
         END IF
      IF ((RQUICK) .AND. (DOCRT.LE.0.0)) CALL RELPOP (IRET, SCBUFF,
     *   IERR)
      IF (IRET.NE.0) GO TO 999
      NEEDRL = (RQUICK) .AND. (DOCRT.GT.0.0)
      IRET = 8
      WASBAD = .FALSE.
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SEQOUT = IROUND (XOUTS)
      DISKOU = IROUND (XOUTD)
      IUSER = NLUSER
      TIME1 = XTIME(1) + ((XTIME(4)/60.0 + XTIME(3))/60.0 +
     *   XTIME(2))/24.0
      TIME2 = XTIME(5) + ((XTIME(8)/60.0 + XTIME(7))/60.0 +
     *   XTIME(6))/24.0
      IF (TIME1.GE.TIME2) TIME2 = 999.
      IF (TIME2.EQ.9999.) XTIME(5) = TIME2
      IF (UVRANG(1).LE.0.0) UVRANG(1) = 0.0
      IF (UVRANG(2).LE.UVRANG(1)) UVRANG(2) = 1.E9
      UVRANG(1) = 1.E6 * UVRANG(1) * UVRANG(1)
      UVRANG(2) = 1.E6 * UVRANG(2) * UVRANG(2)
      IF (CPARM(1).LE.0.0) CPARM(1) = 5.0
      IF (CPARM(2).LE.0.0) CPARM(2) = 5.0
      IF (CPARM(3).LE.0.0) CPARM(3) = 1.E9
      IF (CPARM(4).LE.0.0) CPARM(4) = 1.E9
C                                       Hollerith -> Char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XOUTN, NAMOUT)
      CALL H2CHR (6, 1, XOUTC, CLAOUT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-99.
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=999.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
C                                       Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   IUSER, STAT, SCBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, IUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', SCBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
      CALL COPY (256, CATBLK, CATNEW)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Find weight and scale.
      IF (ISCOMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            IRET = 9
            GO TO 990
            END IF
         END IF
C
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
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
      NCHAN = CATBLK(KINAX+JLOCF)
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      IF ((BCHAN.LE.0) .OR. (BCHAN.GT.NCHAN)) BCHAN = 1
      IF ((ECHAN.LE.0) .OR. (ECHAN.GT.NCHAN)) ECHAN = NCHAN
      IF (BCHAN.GT.ECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         IRET = 1
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       set flag versions
      CALL FNDEXT ('FG', CATBLK, I)
      FGVER = IROUND (XFLAG)
      IF ((FGVER.LT.0) .OR. (FGVER.GT.I)) FGVER = -1
      IF (FGVER.EQ.0) FGVER = I
      FGVERO = IROUND (XFGOUT)
      IF ((DOOUT.GT.0.0) .AND. (FGVERO.EQ.0)) FGVERO = -1
      IF ((FGVERO.EQ.0) .OR. (FGVERO.GT.I)) FGVERO = I + 1
      FGVERI = FGVER
      IF (FGVERO.LE.I) FGVERI = -ABS (FGVERI)
      NFGWRI = 0
      NCFLAG = 0
      NCDEL = 0
      IF (FGVERO.GT.0) DOOUT = -1.0
      REREAD = (FGVERO.GT.0) .AND.
     *   ((UVRANG(1).GT.0.0) .OR. (UVRANG(2).LT.1.E20))
      REREAD = REREAD .OR. (DOOUT.GT.0.0)
      IF ((DOCRT.EQ.0.0) .AND. (FGVERO.LT.0) .AND. (DOOUT.LE.0.0)) THEN
         IRET = 10
         MSGTXT = 'PLEASE ASK ME TO DO SOMETHING: PRINT, FG, OUTPUT?'
         GO TO 990
         END IF
C                                       Max antenna number
      NANT = MAXANT
      CALL FNDEXT ('AN', CATBLK, NVER)
      IF (NVER.GT.0) THEN
         CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, BUFF1, NUMAN, IRET)
         IF (SUBARR.EQ.0) THEN
            IF ((NVER.GT.0) .AND. (IRET.EQ.0)) THEN
               JJ = NUMAN(1)
               NANT = 0
               DO 20 J = 1,JJ
                  NANT = MAX (NANT, NUMAN(J+1))
 20               CONTINUE
               END IF
         ELSE
            NANT = NUMAN(SUBARR+1)
            END IF
         END IF
      SEQIN = CATBLK(KIIMS)
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1035) IRET
         GO TO 990
         END IF
C                                       one of multi-source?
      IF ((NSOUWD.EQ.1) .AND. (DOSWNT)) THEN
         FGSID = SOUWAN(1)
      ELSE
         FGSID = 0
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
      FGLUN = 49
      FGSUBA = SUBARR
      FGFQID = FRQSEL
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      IDATE(1) = -IDATE(1)
      CALL TIMDAT (ITIME, IDATE, CTIME, CDATE)
      WRITE (REASON,1020) TSKNAM, CTIME, CDATE(:9)
      FGTS = TSTART
      FGTE = TEND
      LBCHAN = BCHAN
      LBIF = BIF
C                                       Check spectral channel.
      NPOL = CATBLK(KINAX+JLOCS)
      NCHAN = CATBLK(KINAX+JLOCF)
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
C                                       stokes clips, levels, labels
      CALL LFILL (16, .FALSE., FSTOKE)
      DO 30 I = 1,NPOL
         TEMP = CATD(KDCRV+JLOCS) + (I - CATR(KRCRP+JLOCS)) *
     *      CATR(KRCIC+JLOCS)
         J = IROUND (TEMP) + 9
         ISTOKE(I) = JSTOKE(J)
         FSTOKE(I,I) = .TRUE.
         IF ((J.EQ.4) .OR. (J.EQ.3) .OR. (J.EQ.8) .OR. (J.EQ.7) .OR.
     *      (J.EQ.10)) THEN
            FLUX(I) = CPARM(1)
            RMSL(I) = CPARM(3)
            PTFLUX(I) = CPARM(5)
            FSTOKE(3,I) = .TRUE.
            FSTOKE(4,I) = .TRUE.
            IF (J.EQ.10) FSTOKE(2,I) = .TRUE.
         ELSE
            FLUX(I) = CPARM(2)
            RMSL(I) = CPARM(4)
            PTFLUX(I) = 0.0
            END IF
 30      CONTINUE
C                                       create output image
      IF (DOOUT.GT.0.0) THEN
         CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
         CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT,
     *      SEQOUT)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
         CNOOUT = 1
         FRW(NCFILE+1) = 3
         IRET = 4
         CALL UVCREA (DISKOU, CNOOUT, SCBUFF, IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.NE.2) THEN
               WRITE (MSGTXT,1050) IERR
               GO TO 990
               END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
            IF ((CNOOUT.NE.CNOIN) .OR. (DISKOU.NE.DISKIN)) THEN
               MSGTXT = 'MAY OVERWRITE INPUT FILE ONLY.  QUITTING'
               GO TO 990
               END IF
C                                       Recover existing CATBLK
            FRW(NCFILE+1) = 2
            CALL CATIO ('READ', DISKOU, CNOOUT, CATBLK, 'WRIT', SCBUFF,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1065) IERR
               CALL MSGWRT (6)
               END IF
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKOU
         FCNO(NCFILE) = CNOOUT
         FRW(NCFILE) = FRW(NCFILE) - 1
         CALL COPY (256, CATBLK, CATNEW)
C                                       copy keywords
         CALL KEYCOP (DISKIN, CNOIN, DISKOU, CNOOUT, IERR)
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
      IF (NEEDRL) CALL RELPOP (IRET, SCBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CORER IN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I5)
 1015 FORMAT ('CORER IN: ERROR',I7,' READING HEADER')
 1020 FORMAT (A6,A8,1X,A9)
 1035 FORMAT ('CORER IN: ERROR',I7,' INITIALIZING UV DATA IO')
 1050 FORMAT ('CORER IN: ERROR',I3,' CREATING OUTPUT FILE')
 1065 FORMAT ('CORER IN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE CRERUV (NC, NANT, NP, SVIS, S2VIS, VMAX, VMIN, IRET)
C-----------------------------------------------------------------------
C   CRERUV sends uv data one point at a time to the summing routine
C   and then prints the results.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C                     99 => no bad points found and DOOUT true
C-----------------------------------------------------------------------
      INTEGER   NC, NANT, IRET
      REAL      NP(NC,NANT,NANT), SVIS(NC,NANT,NANT),
     *   S2VIS(NC,NANT,NANT), VMAX(NC,NANT,NANT), VMIN(NC,NANT,NANT)
C
      INCLUDE 'CORER.INC'
      INTEGER   IA1, IA2, ICOR, NUMVIS, RERR, IERR
      REAL      RPARM(20), VIS(3,MAXCIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (BUFF1, VIS)
C-----------------------------------------------------------------------
C                                       Open visibility file
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      NUMVIS = 0
C                                       Zero summing arrays
      ANTMAX = 0
      ICOR = NC * NANT * NANT
      CALL RFILL (ICOR, 0.0, NP)
      CALL RFILL (ICOR, 0.0, SVIS)
      CALL RFILL (ICOR, 0.0, S2VIS)
      CALL RFILL (ICOR, -1.E10, VMAX)
      CALL RFILL (ICOR, +1.E10, VMIN)
      RERR = 0
C                                       Loop
 10   CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            IA1 = RPARM(1+ILOCB) / 256. + 0.1
            IA2 = RPARM(1+ILOCB) - IA1*256. + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         NUMVIS = NUMVIS + 1
         IF ((NUMVIS/10000)*10000.EQ.NUMVIS) THEN
            WRITE (MSGTXT,1010) NUMVIS
            CALL MSGWRT (2)
            END IF
         CALL CORSUM (IA1, IA2, RPARM(1+ILOCU), RPARM(1+ILOCV), VIS,
     *      RERR, NC, NANT, NP, SVIS, S2VIS, VMAX, VMIN)
         GO TO 10
      ELSE IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1015) IRET
         GO TO 990
         END IF
C                                       Print correlator statistics
      CALL COPY (256, CATUV, CATBLK)
      CALL CORPRT (RERR, NC, NANT, NP, SVIS, S2VIS, VMAX, VMIN, IRET)
      IF (IRET.GT.0) GO TO 995
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IERR)
      IF ((DOOUT.GT.0.0) .AND. (NCFLAG.LE.0)) THEN
         MSGTXT = 'NO FLAGS FOUND: DELETE OUTPUT FILE'
         IRET = 99
      ELSE
         IRET = 0
         END IF
      IF (NCFLAG.LE.0) REREAD = .FALSE.
C                                       Error
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C                                       Resume deferred AIPS
 995  IF (NEEDRL) CALL RELPOP (IRET, BUFF1, IA1)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CORER UV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('Just read uv point #',I9)
 1015 FORMAT ('CORER UV: ERROR',I3,' READING VIS FILE')
      END
      SUBROUTINE CORSUM (IA1, IA2, SU, SV, VIS, RERR, NC, NANT, NP,
     *   SVIS, S2VIS, VMAX, VMIN)
C-----------------------------------------------------------------------
C   CORSUM sums the correlator values
C   Inputs:
C      IA1        I    First antenna number
C      IA2        I    Second antenna number
C      SU         R    U spacing in wavelengths
C      SV         R    V spacing in wavelengths
C      VIS(3,*)   R    Visibilities in order real, imaginary, weight
C      NC      I      Number correlators
C      NANT    I      Max antenna number allowed
C   In/Out:
C      RERR       I    Cumulative count of antenna numbering errors
C                      NOTE: For the moment at least we assume that
C                      IA1 < IA2 is required.  If needed, we could
C                      switch IA1 and IA2 around, leaving the phases
C                      alone, when IA1 > IA2 (assuming that for any
C                      given pair the order is always the same).
C      NP      R(*)   Sums of weight and count
C      SVIS    R(*)   Sum of re and im vis (NC,NANT,NANT)
C      S2VIS   R(*)   Sum of re and im vis squared (NC,NANT,NANT)
C      VMAX    R(*)   Max of re and im vis (NC,NANT,NANT)
C      VMIN    R(*)   Min of re and im vis (NC,NANT,NANT)
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2, RERR, NC, NANT
      REAL      SU, SV, VIS(3,*), NP(NC,NANT,NANT),
     *   SVIS(NC,NANT,NANT), S2VIS(NC,NANT,NANT), VMAX(NC,NANT,NANT),
     *   VMIN(NC,NANT,NANT)
C
      INTEGER   ICOR, ISP1, ISP2, I, LPOL
      REAL      WT, AV
      INCLUDE 'CORER.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                      check spacing
      WT = SU * SU + SV * SV
      IF ((WT.LT.UVRANG(1)) .OR. (WT.GT.UVRANG(2))) GO TO 999
C                                      Are antenna nos. okay
      ISP1 = IA1
      ISP2 = IA2
      IF (ISP1.GT.ISP2) THEN
         I = ISP1
         ISP1 = ISP2
         ISP2 = I
         END IF
      IF ((ISP1.LE.0) .OR. (ISP1.GT.NANT) .OR. (ISP2.LE.0) .OR.
     *   (ISP2.GT.NANT)) THEN
         RERR = RERR + 1
         IF (RERR.LE.10.0) THEN
            WRITE (MSGTXT,1000) ISP1, ISP2
            CALL MSGWRT (6)
            END IF
         GO TO 999
         END IF
      ANTMAX = MAX (ANTMAX, IA1)
      ANTMAX = MAX (ANTMAX, IA2)
      LPOL = 0
      DO 40 ICOR = 1,NC
         LPOL = LPOL + 1
         IF (LPOL.GT.NPOL) LPOL = 1
         WT = VIS(3,ICOR)
C                                      Is the weight okay
         IF (WT.GT.0.0) THEN
C                                       Increment counter
            NP(ICOR,ISP1,ISP2) = NP(ICOR,ISP1,ISP2) + WT
            NP(ICOR,ISP2,ISP1) = NP(ICOR,ISP2,ISP1) + 1.0
C                                       real part
            AV = VIS(1,ICOR)
            AV = AV - PTFLUX(LPOL)
            SVIS(ICOR,ISP1,ISP2) = SVIS(ICOR,ISP1,ISP2) + AV*WT
            S2VIS(ICOR,ISP1,ISP2) = S2VIS(ICOR,ISP1,ISP2)
     *         + WT * AV * AV
            VMAX(ICOR,ISP1,ISP2) = MAX (VMAX(ICOR,ISP1,ISP2), AV)
            VMIN(ICOR,ISP1,ISP2) = MIN (VMIN(ICOR,ISP1,ISP2), AV)
C                                       imaginary part
            AV = VIS(2,ICOR)
            SVIS(ICOR,ISP2,ISP1) = SVIS(ICOR,ISP2,ISP1) + AV*WT
            S2VIS(ICOR,ISP2,ISP1) = S2VIS(ICOR,ISP2,ISP1)
     *         + WT * AV * AV
            VMAX(ICOR,ISP2,ISP1) = MAX (VMAX(ICOR,ISP2,ISP1), AV)
            VMIN(ICOR,ISP2,ISP1) = MIN (VMIN(ICOR,ISP2,ISP1), AV)
            END IF
 40      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Antenna pair',I5,' x',I5,' illegal')
      END
      SUBROUTINE CORPRT (RERR, NC, NANT, NP, SVIS, S2VIS, VMAX, VMIN,
     *   IRET)
C-----------------------------------------------------------------------
C   CORPRT does display of bad correlators, also flags data if not
C   dependent on UVRANGE.
C   Inputs:
C      NC      I      Number correlators
C      NANT    I      Max antenna number allowed
C      NP      R(*)   Sums of weight and count
C      SVIS    R(*)   Sum of re and im vis (NC,NANT,NANT)
C      S2VIS   R(*)   Sum of re and im vis squared (NC,NANT,NANT)
C      VMAX    R(*)   Max of re and im vis (NC,NANT,NANT)
C      VMIN    R(*)   Min of re and im vis (NC,NANT,NANT)
C   Inputs from COMMON
C      CATBLK(256)I    Catalog header record.
C   In/Out:
C      RERR       I    Cumulative count of antenna numbering errors
C                      NOTE: For the moment at least we assume that
C                      IA1 < IA2 is required.  If needed, we could
C                      switch IA1 and IA2 around, leaving the phases
C                      alone, when IA1 > IA2 (assuming that for any
C                      given pair the order is always the same).
C   Output: IRET  I    0 => ok, > 0 => failure of printing
C   Outputs to COMMON:
C      NP(4,28,28) R    Sum of weights so far (until last call)
C                       On last call = 1.0 if printed, -1.0 if not
C-----------------------------------------------------------------------
      INTEGER   RERR, NC, NANT, IRET
      REAL      NP(NC,NANT,NANT), SVIS(NC,NANT,NANT),
     *   S2VIS(NC,NANT,NANT), VMAX(NC,NANT,NANT), VMIN(NC,NANT,NANT)
C
      CHARACTER PREFIX*5, TITL1*132, TITL2*132, CLINE*132, SCRTCH*132,
     *   XNAM*12, XCLAS*6
      INTEGER   ICOR, ISP1, ISP2, LUNP, FINDP, LINE, IPAGE, I, ITIME(8),
     *   IROUND, JERR, NACROS, OTYPE, LIF, LCHAN, LPOL, NF(4), TNF(4),
     *   JCOR
      REAL      S, S2, WT, RMS, RMS2, XMAX, SIG, SIG2, AMULT, VX, VX2,
     *   VN, VN2, BP10, WT2, INCRT
      LOGICAL   FLAG, DOFLAG, FIRST
      INCLUDE 'CORER.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Compute results, find max
      XMAX = 0.0
      DO 30 ISP1 = 1,ANTMAX-1
         DO 20 ISP2 = ISP1+1,ANTMAX
            DO 10 ICOR = 1,NC
               WT = NP(ICOR,ISP1,ISP2)
               WT2 = NP(ICOR,ISP2,ISP1)
               IF ((WT2.GE.1.0) .AND. (WT.GT.0.0)) THEN
                  S = SVIS(ICOR,ISP1,ISP2) / WT
                  SVIS(ICOR,ISP1,ISP2) = S
                  RMS = S2VIS(ICOR,ISP1,ISP2) / WT - S * S
                  IF (RMS.GE.0.0) RMS = SQRT(RMS)
                  IF (RMS.LT.0.0) RMS = -SQRT(-RMS)
                  S2VIS(ICOR,ISP1,ISP2) = RMS
                  XMAX = MAX (XMAX, S)
                  XMAX = MAX (XMAX, RMS)
                  XMAX = MAX (XMAX, VMAX(ICOR,ISP1,ISP2))
                  XMAX = MAX (XMAX, -VMIN(ICOR,ISP1,ISP2))
                  S = SVIS(ICOR,ISP2,ISP1) / WT
                  SVIS(ICOR,ISP2,ISP1) = S
                  RMS = S2VIS(ICOR,ISP2,ISP1) / WT - S * S
                  IF (RMS.GE.0.0) RMS = SQRT(RMS)
                  IF (RMS.LT.0.0) RMS = -SQRT(-RMS)
                  S2VIS(ICOR,ISP2,ISP1) = RMS
                  XMAX = MAX (XMAX, S)
                  XMAX = MAX (XMAX, RMS)
                  XMAX = MAX (XMAX, VMAX(ICOR,ISP2,ISP1))
                  XMAX = MAX (XMAX, -VMIN(ICOR,ISP2,ISP1))
                  END IF
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
      IF (XMAX.LE.0.0) THEN
         MSGTXT = 'NO DATA FOUND'
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
C                                       Get scaling for print
      S = XMAX
      CALL METSCA (S, PREFIX, FLAG)
      AMULT = S / XMAX
      DO 35 I = 1,8
         ITIME(I) = IROUND (XTIME(I))
 35      CONTINUE
C                                       Get header info
      CALL H2CHR (12, 1, CATH(KHIMN), XNAM)
      CALL H2CHR (6, 1, CATH(KHIMC), XCLAS)
C                                       Open printer
      IPAGE = 0
      FINDP = 0
      IF (DOCRT.NE.0.0) THEN
         IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
         CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, SCBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) IRET
            CALL MSGWRT (6)
            GO TO 999
            END IF
         OTYPE = 1
         IF (NACROS.LT.95) OTYPE = 2
         IF (NACROS.LT.80) OTYPE = 3
         IF (S.LT.10.0) OTYPE = OTYPE + 3
         END IF
C                                       turn off detailed print if needed
      INCRT = DOCRT
      IF (PRTLEV.LE.0) DOCRT = 0.0
      ICOR = 0
      DO 150 LIF = 1,NIF
         FGBIF = LBIF + LIF - 1
         FIRST = .TRUE.
         DO 140 LCHAN = 1,NCHAN
            FGBCH = LBCHAN + LCHAN - 1
            FGECH = FGBCH
            DO 130 LPOL = 1,NPOL
               ICOR = ICOR + 1
C                                       New titles and force page
               IF (DOCRT.NE.0.0) THEN
                  LINE = 900
                  WRITE (TITL1,1120) XNAM, XCLAS, CATBLK(KIIMS), FGBCH,
     *               FGBIF, ISTOKE(LPOL), PREFIX
                  IF ((OTYPE.EQ.1) .OR. (OTYPE.EQ.4)) THEN
                     WRITE (TITL2,1121)
                  ELSE IF ((OTYPE.EQ.2) .OR. (OTYPE.EQ.5)) THEN
                     WRITE (TITL2,1122)
                  ELSE
                     WRITE (TITL2,1123)
                     END IF
                  IF (DOCRT.LE.-2.5) THEN
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, TITL1, LINE, IPAGE, SCRTCH, IRET)
                     IF (IRET.EQ.0) CALL PRTLIN (LUNP, FINDP, DOCRT,
     *                  NACROS, TITL1, TITL2,TITL2, LINE, IPAGE, SCRTCH,
     *                  IRET)
                     END IF
                  IF ((TIME1.GT.0.0) .OR. (TIME2.LT.999.) .OR.
     *               (PTFLUX(LPOL).NE.0.0)) THEN
                     IF (DOCRT.GT.-2.5) THEN
                        CLINE = ' '
                        IF (IRET.EQ.0) CALL PRTLIN (LUNP, FINDP, DOCRT,
     *                     NACROS, TITL1, TITL2, CLINE, LINE, IPAGE,
     *                     SCRTCH, IRET)
                        END IF
                     WRITE (CLINE,1124) ITIME
                     IF (IRET.EQ.0) CALL PRTLIN (LUNP, FINDP, DOCRT,
     *                  NACROS, TITL1, TITL2, CLINE, LINE, IPAGE,
     *                  SCRTCH, IRET)
                     WRITE (CLINE,1125) PTFLUX(LPOL)
                     IF (IRET.EQ.0) CALL PRTLIN (LUNP, FINDP, DOCRT,
     *                  NACROS, TITL1, TITL2, CLINE, LINE, IPAGE,
     *                  SCRTCH, IRET)
                     END IF
C                                       Check error
                  FIRST = .FALSE.
                  IF (IRET.GT.0) GO TO 980
                  IF (IRET.LT.0) DOCRT = 0.0
                  END IF
               DO 120 ISP1 = 1,ANTMAX-1
                  FGANT1 = ISP1
                  DO 110 ISP2 = ISP1+1,ANTMAX
                     WT = NP(ICOR,ISP1,ISP2)
                     NP(ICOR,ISP1,ISP2) = -1.0
                     WT2 = NP(ICOR,ISP2,ISP1)
                     IF ((WT2.GE.1.0) .AND. (WT.GT.0.0)) THEN
                        S  = SVIS(ICOR,ISP1,ISP2) * AMULT
                        S2 = SVIS(ICOR,ISP2,ISP1) * AMULT
                        RMS  = S2VIS(ICOR,ISP1,ISP2) * AMULT
                        RMS2 = S2VIS(ICOR,ISP2,ISP1) * AMULT
                        WT2 = MAX (1.0, WT2-1.0)
                        WT2 = FLUX(LPOL) * AMULT / SQRT (WT2)
                        SIG = S2VIS(ICOR,ISP1,ISP2) * WT2
                        SIG2 = S2VIS(ICOR,ISP2,ISP1) * WT2
                        BP10 = RMSL(LPOL) * AMULT
                        IF (((RMS.GT.0.0) .OR. (RMS2.GT.0.0)) .AND.
     *                     ((ABS(S).GE.SIG) .OR. (ABS(S2).GE.SIG2) .OR.
     *                     (RMS.GE.BP10) .OR. (RMS2.GE.BP10))) THEN
                           NP(ICOR,ISP1,ISP2) = 1.0
                           WASBAD = .TRUE.
                           NCFLAG = NCFLAG + 1
C                                       Printed line
                           IF (DOCRT.NE.0.0) THEN
                              VX  = VMAX(ICOR,ISP1,ISP2) * AMULT
                              VX2 = VMAX(ICOR,ISP2,ISP1) * AMULT
                              VN  = VMIN(ICOR,ISP1,ISP2) * AMULT
                              VN2 = VMIN(ICOR,ISP2,ISP1) * AMULT
                              IF (OTYPE.EQ.1) THEN
                                 WRITE (CLINE,1130) ISP1, ISP2, WT, S,
     *                              RMS, S2, RMS2, VX, VN, VX2, VN2
                              ELSE IF (OTYPE.EQ.2) THEN
                                 WRITE (CLINE,1131) ISP1, ISP2, WT, S,
     *                              RMS, S2, RMS2, VX, VN, VX2, VN2
                              ELSE IF (OTYPE.EQ.3) THEN
                                 WRITE (CLINE,1132) ISP1, ISP2, S, RMS,
     *                              S2, RMS2, VX, VN, VX2, VN2
                              ELSE IF (OTYPE.EQ.4) THEN
                                 WRITE (CLINE,1133) ISP1, ISP2, WT, S,
     *                              RMS, S2, RMS2, VX, VN, VX2, VN2
                              ELSE IF (OTYPE.EQ.5) THEN
                                 WRITE (CLINE,1134) ISP1, ISP2, WT, S,
     *                              RMS, S2, RMS2, VX, VN, VX2, VN2
                              ELSE IF (OTYPE.EQ.6) THEN
                                 WRITE (CLINE,1135) ISP1, ISP2, S, RMS,
     *                              S2, RMS2, VX, VN, VX2, VN2
                                 END IF
                              CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *                           TITL1, TITL2, CLINE, LINE, IPAGE,
     *                           SCRTCH, IRET)
                              IF (IRET.GT.0) GO TO 980
                              IF (IRET.LT.0) DOCRT = 0.0
                              END IF
                           END IF
                        END IF
 110                 CONTINUE
 120              CONTINUE
 130           CONTINUE
 140        CONTINUE
 150     CONTINUE
C                                       do flagging
      DOFLAG = (FGVERO.GT.0) .AND. (.NOT.REREAD)
      IF (DOFLAG) THEN
         DO 250 ISP1 = 1,ANTMAX-1
            FGANT1 = ISP1
            DO 240 ISP2 = ISP1+1,ANTMAX
               FGANT2 = ISP2
               DO 230 LIF = 1,NIF
                  FGBIF = LBIF + LIF - 1
                  FGEIF = FGBIF
                  DO 220 LPOL = 1,NPOL
                     FLAG = .FALSE.
                     JCOR = (LIF - 1) * NPOL * NCHAN + LPOL
                     DO 210 LCHAN = 1,NCHAN
                        ICOR = JCOR + (LCHAN -1) * NPOL
C                                       bad point
                        IF (NP(ICOR,ISP1,ISP2).GT.0) THEN
                           IF (FLAG) THEN
                              FGECH = LBCHAN + LCHAN - 1
                           ELSE
                              FGBCH = LBCHAN + LCHAN - 1
                              FGECH = FGBCH
                              FLAG = .TRUE.
                              END IF
C                                       good point
                        ELSE
C                                       write flag table
                           IF (FLAG) THEN
                              CALL FLAGFG ('FLAG', FGLUN, DISKIN, CNOIN,
     *                           FGVERI, FGVERO, LFGRNO, FGKOLS, FGNUMV,
     *                           FGSID, FGSUBA, FGFQID, FGANT1, FGANT2,
     *                           FGTS, FGTE, FGBIF, FGEIF, FGBCH, FGECH,
     *                           FSTOKE(1,LPOL), REASON, CATBLK, FGBUFF,
     *                           IRET)
                              IF (IRET.NE.0) GO TO 970
                              NFGWRI = NFGWRI + 1
                              FLAG = .FALSE.
                              END IF
                           END IF
 210                    CONTINUE
C                                       write flag table
                     IF (FLAG) THEN
                        CALL FLAGFG ('FLAG', FGLUN, DISKIN, CNOIN,
     *                     FGVERI, FGVERO, LFGRNO, FGKOLS, FGNUMV,
     *                     FGSID, FGSUBA, FGFQID, FGANT1, FGANT2,
     *                     FGTS, FGTE, FGBIF, FGEIF, FGBCH, FGECH,
     *                     FSTOKE(1,LPOL), REASON, CATBLK, FGBUFF, IRET)
                        IF (IRET.NE.0) GO TO 970
                        NFGWRI = NFGWRI + 1
                        END IF
 220                 CONTINUE
 230              CONTINUE
 240           CONTINUE
 250        CONTINUE
         END IF
C                                       summary print out
      FIRST = (PRTLEV.GT.0.0) .AND. (DOCRT.EQ.0.0)
      DOCRT = INCRT
C                                       # baselines by corr
      IF (DOCRT.NE.0.0) THEN
         CALL FILL (4, 0, TNF)
         LINE = 900
         IF (FIRST) LINE = 999
         WRITE (TITL1,1300) XNAM, XCLAS, CATBLK(KIIMS)
         WRITE (TITL2,1301) (ISTOKE(LPOL), LPOL = 1,NPOL)
         ICOR = 0
         DO 350 LIF = 1,NIF
            FGBIF = LBIF + LIF - 1
            DO 340 LCHAN = 1,NCHAN
               FGBCH = LBCHAN + LCHAN - 1
               FLAG = .FALSE.
               DO 330 LPOL = 1,NPOL
                  ICOR = ICOR + 1
                  NF(LPOL) = 0
                  DO 320 ISP1 = 1,ANTMAX-1
                     DO 310 ISP2 = ISP1+1,ANTMAX
                        IF (NP(ICOR,ISP1,ISP2).GT.0) THEN
                           NF(LPOL) = NF(LPOL) + 1
                           END IF
 310                    CONTINUE
 320                 CONTINUE
                  IF (NF(LPOL).GT.0) FLAG = .TRUE.
                  TNF(LPOL) = TNF(LPOL) + NF(LPOL)
 330              CONTINUE
               IF (FLAG) THEN
                  WRITE (CLINE,1330) FGBIF, FGBCH, (NF(LPOL),
     *               LPOL = 1,NPOL)
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               CLINE, LINE, IPAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 980
                  END IF
 340           CONTINUE
 350        CONTINUE
         IF ((TNF(1).GT.0) .OR. (TNF(2).GT.0) .OR. (TNF(3).GT.0) .OR.
     *      (TNF(4).GT.0)) THEN
            CLINE = ' '
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         CLINE, LINE, IPAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 980
            WRITE (CLINE,1350) (TNF(LPOL), LPOL = 1,NPOL)
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         CLINE, LINE, IPAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 980
            END IF
         END IF
      IF ((DOCRT.NE.0.0) .AND. (NCHAN.GT.2)) THEN
         LINE = 900
         WRITE (TITL1,1400) XNAM, XCLAS, CATBLK(KIIMS)
         WRITE (TITL2,1301) (ISTOKE(LPOL), LPOL = 1,NPOL)
         DO 450 ISP1 = 1,ANTMAX-1
            DO 440 ISP2 = ISP1+1,ANTMAX
               ICOR = 0
               DO 430 LIF = 1,NIF
                  FGBIF = LIF - 1 + LBIF
                  CALL FILL (4, 0, NF)
                  FLAG = .FALSE.
                  DO 420 LCHAN = 1,NCHAN
                     DO 410 LPOL = 1,NPOL
                        ICOR = ICOR + 1
                        IF (NP(ICOR,ISP1,ISP2).GT.0) THEN
                           NF(LPOL) = NF(LPOL) + 1
                           END IF
 410                    CONTINUE
 420                 CONTINUE
                  IF ((NF(1).GT.0) .OR. (NF(2).GT.0) .OR. (NF(3).GT.0)
     *               .OR. (NF(4).GT.0)) THEN
                     WRITE (CLINE,1430) ISP1, ISP2, FGBIF, (NF(LPOL),
     *                     LPOL = 1,NPOL)
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, CLINE, LINE, IPAGE, SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 980
                     END IF
 430              CONTINUE
 440           CONTINUE
 450        CONTINUE
         END IF
      GO TO 980
C                                       FG error
 970  WRITE (MSGTXT,1970) IRET, LFGRNO
      CALL MSGWRT (8)
C                                       Close down
 980  IF (FINDP.NE.0) CALL LPCLOS (LUNP, FINDP, LINE, JERR)
      IF (IRET.LT.0) IRET = 0
C                                       Antenna errors
      IF (RERR.GT.0) THEN
         WRITE (MSGTXT,1980) RERR
         CALL MSGWRT (5)
         END IF
C                                       clse FG
      IF ((DOFLAG) .AND. (NCFLAG.GT.0)) THEN
         CALL FLAGFG ('CLOS', FGLUN, DISKIN, CNOIN, FGVERI, FGVERO,
     *      LFGRNO, FGKOLS, FGNUMV, FGSID, FGSUBA, FGFQID, FGANT1,
     *      FGANT2, FGTS, FGTE, FGBIF, FGEIF, FGBCH, FGECH, FSTOKE,
     *      REASON, CATBLK, FGBUFF, JERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1110 FORMAT ('OPEN PRINTER ERROR',I7)
 1120 FORMAT (A12,'.',A6,'.',I4,4X,'Corr. average chan/IF', I4,'/',I2,
     *   ' pol ',A2,4X,'in ',A5,' Jy')
 1121 FORMAT (' Ant',5X,'Weight',5X,'Av  Real  Rms',7X,'Av  Imag  Rms',
     *   7X,'Max  Real  Min',6X,'Max  Imag  Min')
 1122 FORMAT (' Ant',4X,'Weight',3X,'Av  Real  Rms',3X,'Av  Imag  Rms',
     *   3X,'Max  Real  Min',3X,'Max  Imag  Min')
 1123 FORMAT (' Ant',4X,'Av  Real  Rms',3X,'Av  Imag  Rms',
     *   3X,'Max  Real  Min',3X,'Max  Imag  Min')
 1124 FORMAT ('Times from',I6,'/',I2.2,':',I2.2,':',I2.2,4X,'to',I6,
     *   '/',I2.2,':',I2.2,':',I2.2)
 1125 FORMAT ('With a',F10.6,' Jansky point source at the origin ',
     *   'subtracted')
 1130 FORMAT (I2,'-',I2,F10.0,4(F11.3,F9.3))
 1131 FORMAT (I2,'-',I2,F9.0,2(F9.3,F7.3),2(F9.3,F8.3))
 1132 FORMAT (I2,'-',I2,2(F9.3,F7.3),2(F9.3,F8.3))
 1133 FORMAT (I2,'-',I2,F10.0,4(F11.4,F9.4))
 1134 FORMAT (I2,'-',I2,F9.0,2(F9.4,F7.4),2(F9.4,F8.4))
 1135 FORMAT (I2,'-',I2,2(F9.4,F7.4),2(F9.4,F8.4))
 1300 FORMAT (A12,'.',A6,'.',I4,4X,'Number baselines flagged by IF and',
     *   ' channel')
 1301 FORMAT (40X,4(7X,A2))
 1330 FORMAT ('IF',I3,'  channel',I6,'  flagged baselines:',4I9)
 1350 FORMAT ('Total over all IFs, channels, baselines:',4I9)
 1400 FORMAT (A12,'.',A6,'.',I4,4X,'Number channels flagged by ',
     *   'baseline and IF')
 1430 FORMAT ('BL',I4,'  -',I4,'   IF',I3,'  flagged channels:',4I9)
 1970 FORMAT ('CORPRT: ERROR',I3,' WRITING FLAG TABLE AT REC',I10)
 1980 FORMAT ('There were',I8,' antenna numbering errors')
      END
      SUBROUTINE CRERRE (NC, NANT, NP, IRET)
C-----------------------------------------------------------------------
C   CRERRE sends uv data one point at a time to the flagging routine
C   and writes either flags or data.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, NANT, IRET
      REAL      NP(NC,NANT,NANT)
C
      INCLUDE 'CORER.INC'
      INTEGER   IERR, NUMVIS, INIO, IPTRI, LUNI, I, IBIND, INDI, LENBU,
     *   VO, BO, NMCOR, LUNO, INDO, OBIND, IPTRO, NIOLIM, NIOUT, XCOUNT,
     *   JERR
      REAL      BUFFC(UVBFSS)
      CHARACTER IFILE*48
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA VO, BO, LENBU /0, 1, 16/
      DATA LUNI, LUNO /16, 17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       recover header
      CALL COPY (256, CATNEW, CATBLK)
      CALL UVPGET (IERR)
      XCOUNT = 0
C                                       Open visibility file
      CALL ZPHFIL ('UV', DISKIN, CNOIN, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'READ'
         GO TO 990
         END IF
C                                       Init vis file for read.
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, LENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'READ'
         GO TO 990
         END IF
C                                       init output
      IF (DOOUT.GT.0.0) THEN
         CALL ZPHFIL ('UV', DISKOU, CNOOUT, 1, IFILE, IRET)
         CALL ZOPEN (LUNO, INDO, DISKOU, IFILE, T, F, F, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN', 'WRITE'
            GO TO 990
            END IF
C                                       Init vis file for read.
         CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, LENBU, JBUFSZ,
     *      BUFF2, BO, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT', 'WRITE'
            GO TO 990
            END IF
         IPTRO = OBIND
         NIOUT = 0
         NIOLIM = LENBU
         END IF
      NUMVIS = 0
      NMCOR = (LREC - NRPARM) / CATBLK(KINAX)
C                                       Read loop
 100  CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'READ'
         GO TO 990
      ELSE IF (INIO.GT.0) THEN
         IPTRI = IBIND
         DO 150 I = 1,INIO
            NUMVIS = NUMVIS + 1
            IF ((NUMVIS/10000)*10000.EQ.NUMVIS) THEN
               WRITE (MSGTXT,1110) NUMVIS
               CALL MSGWRT (2)
               END IF
C                                      Call flagging routine.
C                                      Compressed data.
            IF (ISCOMP) THEN
               CALL ZUVXPN (NMCOR, BUFF1(IPTRI+NRPARM),
     *            BUFF1(IPTRI+ILOCWT), BUFFC)
               CALL CORDEL (BUFF1(IPTRI), BUFFC, NC, NANT, NP, IRET)
            ELSE
               CALL CORDEL (BUFF1(IPTRI), BUFF1(IPTRI+NRPARM), NC, NANT,
     *            NP, IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            IF ((IRET.EQ.0) .AND. (DOOUT.GT.0.0)) THEN
               XCOUNT = XCOUNT + 1
C                                       Compressed
               IF (ISCOMP) THEN
                  CALL RCOPY (NRPARM, BUFF1(IPTRI), BUFF2(IPTRO))
                  CALL ZUVPAK (NMCOR, BUFFC, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPARM))
               ELSE
                  CALL RCOPY (LREC, BUFF1(IPTRI), BUFF2(IPTRO))
                  END IF
               IPTRO = IPTRO + LREC
               NIOUT = NIOUT + 1
C                                       Write vis record.
               IF (NIOUT.GE.NIOLIM) THEN
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, OBIND,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1100) IRET, 'WRITE'
                     GO TO 990
                     END IF
                  IPTRO = OBIND
                  NIOUT = 0
                  END IF
               END IF
            IPTRI = IPTRI + LREC
 150        CONTINUE
         GO TO 100
         END IF
C                                       Finish write
      IF (DOOUT.GT.0.0) THEN
         NIOUT = - NIOUT
         CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FLSH', 'WRITE'
            GO TO 990
            END IF
C                                       Compress output file.
         NVIS = XCOUNT
         CALL UCMPRS (NVIS, DISKOU, CNOOUT, LUNO, CATBLK, IRET)
C                                       close up flag table
      ELSE
         CALL FLAGFG ('CLOS', FGLUN, DISKIN, CNOIN, FGVERI, FGVERO,
     *      LFGRNO, FGKOLS, FGNUMV, FGSID, FGSUBA, FGFQID, FGANT1,
     *      FGANT2, FGTS, FGTE, FGBIF, FGEIF, FGBCH, FGECH, FSTOKE,
     *      REASON, CATBLK, FGBUFF, JERR)
         END IF
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      IF (DOOUT.GT.0.0) CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CORER RE: ERROR',I3,' ON ',A,' FOR ',A,' VIS FILE')
 1100 FORMAT ('CORER RE: ERROR',I3,' ON ',A,' VIS FILE')
 1110 FORMAT ('Just re-read uv point #',I9)
      END
      SUBROUTINE CORDEL (RPARM, VIS, NC, NANT, NP, IRET)
C-----------------------------------------------------------------------
C   CORDEL deletes those correlators for which NP > 0
C   Inputs:
C      RPARM   R(*)     Random parameters
C      NC      I        Number correlators in NP
C      NANT    I        Number antennas in NP
C      NP      R(*)     Flagging info (NC,NANT,NANT)
C   In/Out:
C      VIS     R(3,*)   Visibilities in order real, imaginary, weight
C   Output:
C      IRET    I        Error code: 0 -> ok, -1 -> skip these vis
C-----------------------------------------------------------------------
      INTEGER   NC, NANT, IRET
      REAL      RPARM(*), VIS(3,*), NP(NC,NANT,NANT)
C
      INTEGER   ICOR, ISP1, ISP2, I, LIF, LCHAN, LPOL, INDX, INCSI,
     *   INCFI, INCIFI, JCOR
      REAL      WT, EPS
      LOGICAL   FLAG
      INCLUDE 'CORER.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA EPS /0.5E-6/
C-----------------------------------------------------------------------
      IRET = 0
C                                      Check times
      IF (RPARM(1+ILOCT).LT.TIME1) GO TO 999
      IF (RPARM(1+ILOCT).GT.TIME2) GO TO 999
C                                      check baseline length
      WT = RPARM(1+ILOCU)**2 + RPARM(1+ILOCV)**2
      IF ((WT.LT.UVRANG(1)) .OR. (WT.GT.UVRANG(2))) GO TO 999
C                                      Are antenna nos. okay, subarray
      IF (ILOCB.GE.0) THEN
         ISP1 = RPARM(1+ILOCB) / 256.0 + 0.1
         ISP2 = (RPARM(1+ILOCB) - 256*ISP1) + 0.1
         I = 100.0 * (RPARM(1+ILOCB) - 256*ISP1 - ISP2) + 1.01
      ELSE
         ISP1 = RPARM(1+ILOCA1) + 0.1
         ISP2 = RPARM(1+ILOCA2) + 0.1
         I = RPARM(1+ILOCSA) + 0.1
         END IF
      IF ((FGSUBA.GT.0) .AND. (I.NE.FGSUBA)) GO TO 999
C                                       source
      IF ((ILOCSU.GE.0) .AND. (FGSID.GT.0)) THEN
         I = RPARM(1+ILOCSU) + 0.1
         IF (I.NE.FGSID) GO TO 999
         END IF
C                                       fqid
      IF ((ILOCFQ.GE.0) .AND. (FGFQID.GT.0)) THEN
         I = RPARM(1+ILOCFQ) + 0.1
         IF (I.NE.FGFQID) GO TO 999
         END IF
      IF (DOOUT.GT.0.0) THEN
         INCSI = INCS / CATBLK(KINAX)
         INCFI = INCF / CATBLK(KINAX)
         INCIFI = INCIF / CATBLK(KINAX)
      ELSE
         FGTS = RPARM(1+ILOCT) - EPS
         FGTE = RPARM(1+ILOCT) + EPS
         END IF
      FGANT1 = ISP1
      FGANT2 = ISP2
      ICOR = 0
      DO 50 LIF = 1,NIF
         FGBIF = LBIF + LIF - 1
         FGEIF = FGBIF
         DO 40 LPOL = 1,NPOL
            JCOR = (LIF -1) * NPOL * NCHAN + LPOL
            FLAG = .FALSE.
            DO 30 LCHAN = 1,NCHAN
               ICOR = JCOR + (LCHAN-1) * NPOL
               FGBCH = LBCHAN + LCHAN - 1
C                                       bad point
               IF (NP(ICOR,ISP1,ISP2).GT.0.0) THEN
C                                       output data
                  IF (DOOUT.GT.0.0) THEN
                     INDX = (FGBIF-1)*INCIFI + (FGBCH-1)*INCFI
     *                  + (LPOL-1)*INCSI + 1
                     VIS(3,INDX) = -ABS (VIS(3,INDX))
                     NCDEL = NCDEL + 1
C                                       FG table info
                  ELSE
                     IF (FLAG) THEN
                        FGECH = FGECH + 1
                     ELSE
                        FGBCH = LBCHAN + LCHAN - 1
                        FGECH = FGBCH
                        FLAG = .TRUE.
                        END IF
                     END IF
C                                       good sample
               ELSE IF (DOOUT.LE.0.0) THEN
                  IF (FLAG) THEN
                     CALL FLAGFG ('FLAG', FGLUN, DISKIN, CNOIN, FGVERI,
     *                  FGVERO, LFGRNO, FGKOLS, FGNUMV, FGSID, FGSUBA,
     *                  FGFQID, FGANT1, FGANT2, FGTS, FGTE, FGBIF,
     *                  FGEIF, FGBCH, FGECH, FSTOKE(1,LPOL), REASON,
     *                  CATBLK, FGBUFF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1970) IRET, LFGRNO
                        CALL MSGWRT (8)
                        GO TO 999
                        END IF
                     NFGWRI = NFGWRI + 1
                     FLAG = .FALSE.
                     END IF
                  END IF
 30            CONTINUE
            IF (FLAG) THEN
               CALL FLAGFG ('FLAG', FGLUN, DISKIN, CNOIN, FGVERI,
     *            FGVERO, LFGRNO, FGKOLS, FGNUMV, FGSID, FGSUBA, FGFQID,
     *            FGANT1, FGANT2, FGTS, FGTE, FGBIF, FGEIF, FGBCH,
     *            FGECH, FSTOKE(1,LPOL), REASON, CATBLK, FGBUFF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1970) IRET, LFGRNO
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               NFGWRI = NFGWRI + 1
               END IF
 40         CONTINUE
 50      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1970 FORMAT ('CORDEL: ERROR',I3,' WRITING FLAG TABLE AT REC',I10)
      END
      SUBROUTINE CRERHI
C-----------------------------------------------------------------------
C   CRERHI copies and updates history file.  It also copies any AN
C   (antenna) and GA (gain) extension files.
C-----------------------------------------------------------------------
      CHARACTER LINE*72, CTIME*8, CDATE*12, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, ITIME(3), TT, IDATE(3), NONOT
      LOGICAL   T
      INCLUDE 'CORER.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /0, ' '/
C-----------------------------------------------------------------------
C                                       Write History.
      IF ((NFGWRI.GT.0) .OR. ((DOOUT.GT.0.0) .AND. (NCFLAG.GT.0))) THEN
         CALL HIINIT (3)
C                                       copy old HI
         IF (DOOUT.GT.0.0) THEN
C                                       Copy/open history file.
            CALL HISCOP (LUN1, LUN2, DISKIN, DISKOU, CNOIN, CNOOUT,
     *         CATBLK, BUFF2, SCBUFF, IERR)
            IF (IERR.GT.2) THEN
               WRITE (MSGTXT,1000) IERR
               CALL MSGWRT (6)
               GO TO 100
               END IF
C                                       New history
         ELSE
            CALL HIOPEN (LUN2, DISKIN, CNOIN, SCBUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR
               CALL MSGWRT (6)
               GO TO 100
               END IF
            CALL ZDATE (IDATE)
            CALL ZTIME (ITIME)
            CALL TIMDAT (ITIME, IDATE, CTIME, CDATE)
            WRITE (LINE,1005) TSKNAM, RLSNAM, CDATE, CTIME
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                       UV range
         UVRANG(1) = SQRT (UVRANG(1)) / 1.E3
         UVRANG(2) = SQRT (UVRANG(2)) / 1.E3
         IF ((UVRANG(1).GT.0.001) .OR. (UVRANG(2).LE.9.9E8)) THEN
            WRITE (LINE,1010) TSKNAM, UVRANG
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                       time
         IF ((TIME1.GT.0.) .OR. (TIME2.LT.9998.5)) THEN
            IF (TIME1.GT.0.) THEN
               TT = TIME1
               ITIME(1) = TT
               TT = 24.0 * (TT - ITIME(1))
               ITIME(2) = TT
               TT = 60.0 * (TT - ITIME(2))
               ITIME(3) = TT
               TT = 60.0 * (TT - ITIME(3))
               WRITE (LINE,1015) TSKNAM, ITIME, TT
            ELSE
               WRITE (LINE,1016) TSKNAM
               END IF
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            IF (TIME2.LT.9998.5) THEN
               TT = TIME2
               ITIME(1) = TT
               TT = 24.0 * (TT - ITIME(1))
               ITIME(2) = TT
               TT = 60.0 * (TT - ITIME(2))
               ITIME(3) = TT
               TT = 60.0 * (TT - ITIME(3))
               WRITE (LINE,1017) TSKNAM, ITIME, TT
            ELSE
               WRITE (LINE,1018) TSKNAM
               END IF
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                       Point source flux
         IF (CPARM(5).NE.0.0) THEN
            WRITE (LINE,1020) TSKNAM, CPARM(5)
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                       Cutoff
         WRITE (LINE,1025) TSKNAM, CPARM(1)
         CALL HIADD (LUN2, LINE, SCBUFF, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (LINE,1026) TSKNAM, CPARM(2)
         CALL HIADD (LUN2, LINE, SCBUFF, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (CPARM(3).LE.9.9E8) THEN
            WRITE (LINE,1027) TSKNAM, CPARM(3)
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
         IF (CPARM(4).LE.9.9E8) THEN
            WRITE (LINE,1028) TSKNAM, CPARM(4)
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                       deletions
         IF (FGVERI.GT.0) THEN
            WRITE (LINE,1030) TSKNAM, FGVERI
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                       output flags
         IF (FGVERO.GT.0) THEN
            WRITE (LINE,1031) TSKNAM, FGVERO
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            WRITE (LINE,1032) TSKNAM, NFGWRI
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            WRITE (MSGTXT,2032) NFGWRI
            CALL MSGWRT (4)
         ELSE
            WRITE (LINE,1033) TSKNAM, NCFLAG
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            WRITE (MSGTXT,2033) NCFLAG
            CALL MSGWRT (4)
            WRITE (LINE,1034) TSKNAM, NCDEL
            CALL HIADD (LUN2, LINE, SCBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            WRITE (MSGTXT,2034) NCDEL
            CALL MSGWRT (4)
            END IF
C                                       Close HI file
 100     CALL HICLOS (LUN2, T, SCBUFF, IERR)
         END IF
C                                       copy tables
      IF ((DOOUT.GT.0.0) .AND. (NCFLAG.GT.0)) THEN
         CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKOU, CNOIN,
     *      CNOOUT, CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            MSGTXT = 'PROBLEM COPYING TABLES TO OUTPUT FILE'
            CALL MSGWRT (7)
            END IF
C                                       Update CATBLK.
         CALL CATIO ('UPDT', DISKOU, CNOOUT, CATBLK, 'REST', BUFF1,
     *         IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CRERHI: ERROR',I3,' OPEN HISTORY FILE')
 1005 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1010 FORMAT (A6,'UVRANGE =',1PE12.4,',',1PE12.4,5X,'/ Klambda UV ',
     *   'limits')
 1015 FORMAT (A6,'/ Checked from time=',I5,2I3.2,F6.2)
 1016 FORMAT (A6,'/ Checked from the beginning')
 1017 FORMAT (A6,'/         to  time=',I5,2I3.2,F6.2)
 1018 FORMAT (A6,'/         to  the end')
 1020 FORMAT (A6,'PTFLUX =',1PE12.4,19X,'/ Jy point source ',
     *   'subtracted')
 1025 FORMAT (A6,'IFLUX =',F8.4,5X,'/ times expected error in ',
     *   'mean = ipol cutoff')
 1026 FORMAT (A6,'XFLUX =',F8.4,5X,'/ times expected error in ',
     *   'mean = xpol cutoff')
 1027 FORMAT (A6,'RMSICUT =',1PE12.4,4X,'/ Jy limit on ipol RMS')
 1028 FORMAT (A6,'RMSXCUT =',1PE12.4,4X,'/ Jy limit on xpol RMS')
 1030 FORMAT (A6,'FGVERI =',I5,5X,'/ FG table vers used and copied')
 1031 FORMAT (A6,'FGVERO =',I5,5X,'/ FG table vers written')
 1032 FORMAT (A6,'NFGWRI =',I10,5X,'/ FG table records written')
 1033 FORMAT (A6,'NCFLAG =',I10,5X,'/ Number correlators flagged')
 1034 FORMAT (A6,'/ Number vis samples flagged',I12)
 2032 FORMAT ('FG table records written  ',I12)
 2033 FORMAT ('Number correlators flagged',I12)
 2034 FORMAT ('Number vis samples flagged',I12)
      END
      SUBROUTINE FLAGFG (OPCODE, LUN, DISK, CNO, VERI, VER, LFGRNO,
     *   FGKOLS, FGNUMV, ID, SUBA, FQID, ANT1, ANT2, BTIME, ETIME, BIF,
     *   EIF, BCHAN, ECHAN, PFLAGS, REASON, CATUV, BUFF, IRET)
C-----------------------------------------------------------------------
C   Updates the Flag (FG) table. Adapted from FLAGUP
C   One entry is made indicating a visibility to be rejected.
C   The FLAG table will be opened on the first call but a final call
C   with OPCODE='CLOS' is required to close the file.
C   Inputs:
C      OPCODE   C*4      Operation desired, 'CLOS'=>close file
C                        Anything else = 'FLAG'
C      LUN      I        Logical unit number to use
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VERI     I        Input version number
C      VER      I        FG file version
C      ID       I        Source ID as defined in SOURCE table
C      SUBA     I        Subarray number.
C      FQID     I        Freqid number
C      ANT1     I        First antenna number in baseline
C      ANT2     I        Second antenna number in baseline
C      BTIME    R        Start time of data to be flagged (Days)
C      ETIME    R        End time of data to be flagged (Days)
C      BIF      I        First IF number to flag. 0=>all
C      EIF      I        Last IF number to flag. 0=>all higher than IFS(1)
C      BCHAN    I        First channel number to flag. 0=>all
C      ECHAN    I        Last channel number to flag. 0=>all higher.
C      PFLAGS   L(4)     Correlator flags
C      REASON   C*24     Reason for flagging blank => ignore for unflag.
C   Input/Output:
C      LFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(*)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C      CATUV    I(256)   Header for disk file to get FG table
C      BUFF     I(512)   I/O buffer and related storage, also defines
C                        file if open.
C   Output:
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, REASON*24
      INTEGER   LUN, DISK, CNO, VERI, VER, LFGRNO, FGKOLS(*), FGNUMV(*),
     *   ID, SUBA, FQID, ANT1, ANT2, BIF, EIF, BCHAN, ECHAN, CATUV(256),
     *   BUFF(*), IRET
      REAL      BTIME, ETIME
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TREAS*24, CTEMP*12
      INTEGER   IDT, SUBT, ANTS(2), IFS(2), CHANS(2), IDUM, FIND, I,
     *   BUFF2(512), LUN2, IFGKOL(MAXFGC), IFGNUM(MAXFGC), NROW, IFQ,
     *   IFGRNO
      LOGICAL   PFLAGS(4), TFLAGS(4), FIRST
      REAL      TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE FIRST
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
C                                       See if table open - check FTAB
      IF (OPCODE.NE.'CLOS') THEN
         FIND = BUFF(82)
C                                       Open file
         IF ((FIND.LT.0) .OR. (FIND.GT.10000) .OR. (LUN.NE.FTAB(FIND)))
     *      THEN
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'CLRD', BUFF, IRET)
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'WRIT', BUFF, IRET)
C
            CALL FLGINI ('WRIT', BUFF, DISK, CNO, VER, CATUV, LUN,
     *         LFGRNO, FGKOLS, FGNUMV, IRET)
C                                       Report on the need for flagging
            WRITE (MSGTXT,1000) VER
            IF (.NOT.FIRST) WRITE (MSGTXT,1001) VER
            CALL MSGWRT (2)
            IF (IRET.NE.0) GO TO 999
C                                       Copy the old file
            IF ((FIRST) .AND. (VERI.GT.0)) THEN
               LUN2 = LUN + 1
               CALL FLGINI ('READ', BUFF2, DISK, CNO, VERI, CATUV, LUN2,
     *            IFGRNO, IFGKOL, IFGNUM, IRET)
               IF (IRET.NE.0) GO TO 999
               NROW = BUFF2(5)
               WRITE (MSGTXT,1002) NROW, VERI, VER
               CALL MSGWRT (2)
               DO 20 I = 1,NROW
                  CALL TABFLG ('READ', BUFF2, IFGRNO, IFGKOL, IFGNUM,
     *               IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *               TREAS, IRET)
                  IF (IRET.GT.0) GO TO 999
                  IF (IRET.EQ.0) THEN
                     CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV,
     *                  IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *                  TREAS, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
 20               CONTINUE
               CALL TABIO ('CLOS', 0, IFGRNO, BUFF2, BUFF2, I)
               END IF
            FIRST = .FALSE.
C                                       Mark as unsorted
            BUFF(43) = 0
            BUFF(44) = 0
            END IF
C                                       Set up for flagging
         ANTS(1) = ANT1
         ANTS(2) = ANT2
         TIMER(1) = BTIME
         TIMER(2) = ETIME
         IFS(1) = BIF
         IFS(2) = EIF
         CHANS(1) = BCHAN
         CHANS(2) = ECHAN
C                                       Flag table entry.
         CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV, ID, SUBA,
     *      FQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
C                                       Close
      ELSE
         CALL TABFLG ('CLOS', BUFF, LFGRNO, FGKOLS, FGNUMV, IDT, SUBT,
     *      FQID, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
C                                       Clear write status
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'CLWR', BUFF, IRET)
C                                       Reset status to read
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'READ', BUFF, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Found some bad data, will write flags to table FG', I4)
 1001 FORMAT ('Found some bad data, will add   flags to table FG', I4)
 1002 FORMAT ('Copy',I8,' rows from FG vers',I3,' to',I3)
      END
