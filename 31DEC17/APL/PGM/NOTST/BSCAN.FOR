LOCAL INCLUDE 'BSCAN.INC'
C                                       Local include for BSCAN
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XANTU(50),
     *   XINVER, CHNSEL(3,10), APARM(10), SCRBUF(256), BUFF2(UVBFSS)
      INTEGER   SEQIN, DISKIN, NUMHIS, JBUFSZ, ILOCWT, CATOLD(256),
     *   INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO, NRPRMI,
     *   NRPRMO, OLDCNO, IANTU(MAXANT), PCVERS, IBUFF2(UVBFSS)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, HISCRD(10)*64
      EQUIVALENCE (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN, XECHAN,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, XANTU, XINVER, CHNSEL, APARM
      COMMON /BSCANP/ CATOLD, SEQIN, DISKIN, NUMHIS, ILOCWT, INCSI,
     *   INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO, NRPRMI, NRPRMO,
     *   ISCOMP, OLDCNO, IANTU, PCVERS
      COMMON /CHARPM/ NAMEIN, CLAIN, HISCRD
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       special scan info
      INTEGER   MAXSCN
      PARAMETER (MAXSCN = 100)
      INTEGER   NOSCAN, NOSTNS, CURSCN, NOANTS(MAXSCN), NOBLNS(MAXSCN),
     *   PCOUNT(MAXSCN)
      REAL      TSCAN(2,MAXSCN)
      DOUBLE PRECISION ISANTS(MAXANT,MAXSCN), ISANTM(MAXSCN)
      COMMON /SCANS/ ISANTS, ISANTM, TSCAN, NOANTS, NOBLNS, NOSCAN,
     *   CURSCN, NOSTNS, PCOUNT
C                                       End local include for BSCAN
LOCAL END
      PROGRAM BSCAN
C-----------------------------------------------------------------------
C! Find best scan for calibration
C# Utility UV VLB Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2012, 2014-2015
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
C   BSCAN looks at each scan to see which has all antennas and hence
C   would be better for calibration (manual phase cal, fringe finding).
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C   full set of calibration adverbs
C      CHANSEL(3,10)  CHNSEL        Start/stop times, num antennas
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR
      INCLUDE 'BSCAN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'BSCAN '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BSCANI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL SENDUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL PTPARM (40, CHNSEL, SCRBUF, IERR)
      CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE BSCANI (PRGN, JERR)
C-----------------------------------------------------------------------
C   BSCANI gets input parameters for BSCAN and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, NFREQ, LUN, LUNTMP, J
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'BSCAN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 237
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .FALSE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Do not restart AIPS
      RQUICK = .FALSE.
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (16, 1, XSOUR, SOURCS(1))
      SELQUA = IROUND (XQUAL)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      PCVERS = IROUND (XINVER)
C                                       init scan info
      CALL FILL (MAXSCN, 0, NOANTS)
      CALL FILL (MAXSCN, 0, NOBLNS)
      I = MAXANT * MAXSCN
      CALL DFILL (I, -1.0D0, ISANTS)
      CURSCN = 0
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
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
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
      LUN = LUNTMP(1)
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
      CALL UVGET ('INIT', RPARM, SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       get list of scans
      LUN = LUNTMP(1)
      CALL GSCANS (LUN, DISKIN, OLDCNO, CATUV, SUBARR, SOUWAN(1),
     *   IBUFF2, NOSCAN, TSCAN)
      IF (NOSCAN.LE.0) THEN
         MSGTXT = 'NO SCANS FOUND FOR ' // SOURCS(1)
         JERR = 8
         GO TO 990
         END IF
C                                       PS table
      CALL FNDEXT ('PC', CATUV, I)
      PCVERS = MIN (PCVERS, I)
      IF (PCVERS.LE.0) THEN
         CALL FILL (NOSCAN, 1, PCOUNT)
      ELSE
         CALL FILL (NOSCAN, 0, PCOUNT)
         LUN = LUNTMP(1)
         CALL GETPCS (LUN, DISKIN, OLDCNO, CATUV, SUBARR, SOUWAN(1),
     *      IBUFF2, NOSCAN, TSCAN, PCVERS, PCOUNT)
         END IF
C                                       set antenna info
      CALL GETANT (DISKIN, OLDCNO, SUBARR, CATUV, BUFF2, JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'ERROR GETTING ANTENNA INFO'
         GO TO 990
         END IF
      CALL FILL (MAXANT, 1, IANTU)
      DO 10 I = 1,50
         J = XANTU(I) + 0.5
         IF ((J.GT.0) .AND. (J.LE.MAXANT)) IANTU(J) = 0
 10      CONTINUE
      NOSTNS = 0
      DO 20 J = 1,NSTNS
         I = INDEX (STNNAM(J), 'OUT')
         IF ((I.LE.0) .AND. (STNNAM(J).NE.' ') .AND. (STNX(J).NE.0.0D0)
     *      .AND. (STNY(J).NE.0.0D0) .AND. (STNZ(J).NE.0.0D0) .AND.
     *      (IANTU(J).GT.0)) THEN
            NOSTNS = NOSTNS + 1
            DO 15 I = 1,NOSCAN
               IF (PCOUNT(I).GT.0) ISANTS(J,I) = 0.0D0
 15            CONTINUE
            END IF
 20      CONTINUE
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BSCANI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE SENDUV (IRET)
C-----------------------------------------------------------------------
C   SENDUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IA1, IA2, NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256)
      INCLUDE 'BSCAN.INC'
      REAL      DUM, BASEN, VIS(UVBFSS), RPARM(20)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN/ININT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      NUMVIS = 0
      XCOUNT = 0
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ VIS RECORDS'
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            IA1 = BASEN / 256. + 0.1
            IA2 = BASEN - IA1*256. + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         NUMVIS = NUMVIS + 1
C                                       call user routine
         CALL DCOUNT (NUMVIS, RPARM(1+ILOCT), IA1, IA2, VIS)
         XCOUNT = XCOUNT + 1.0D0
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       DCOUNT analyze the results
      NUMVIS = -1
      CALL DCOUNT (NUMVIS, DUM, IA1, IA2, VIS)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SENDUV: ERROR',I3,' ON ',A)
      END
      SUBROUTINE DCOUNT (NUMVIS, T, IA1, IA2, VIS)
C-----------------------------------------------------------------------
C   accumulates info about good data
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, analyze the
C                   results
C      T       R    Time in days since 0 IAT on the first day for
C                   which there is data.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C   Output in COMMON:
C      NUMHIS    I         # history entries (max. 10)
C      HISCRD    C(NUMHIS) History records
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2
      REAL      T, VIS(3,*)
C
      INCLUDE 'BSCAN.INC'
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXI, I1, I2, IC, NHOPE
      DOUBLE PRECISION VISCNT(MAXANT,MAXANT), DTEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
C-----------------------------------------------------------------------
      IF (NUMVIS.GT.0) THEN
C                                       pointers to traverse the data
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         NS = MIN (2, NS)
C                                       time to change scans
 10      IF ((CURSCN.LE.0) .OR. (T.GT.TSCAN(2,CURSCN))) THEN
C                                       squirrel away current one
            IF (CURSCN.GT.0) THEN
               DO 40 I2 = 1,NSTNS
                  DO 30 I1 = 1,NSTNS
                     IF ((VISCNT(I1,I2).GT.0.0D0) .AND.
     *                  (ISANTS(I1,CURSCN).GE.0.0D0) .AND.
     *                  (ISANTS(I2,CURSCN).GE.0.0D0)) THEN
                        NOBLNS(CURSCN) = NOBLNS(CURSCN) + 1
                        ISANTS(I1,CURSCN) = ISANTS(I1,CURSCN) +
     *                     VISCNT(I1,I2)
                        ISANTS(I2,CURSCN) = ISANTS(I2,CURSCN) +
     *                     VISCNT(I1,I2)
                        END IF
 30                  CONTINUE
 40               CONTINUE
               ISANTM(CURSCN) = 0.0D0
               DO 50 I1 = 1,NSTNS
                  IF (ISANTS(I1,CURSCN).GT.0.0D0) THEN
                     NOANTS(CURSCN) = NOANTS(CURSCN) + 1
                     ISANTM(CURSCN) = ISANTM(CURSCN) + ISANTS(I1,CURSCN)
                     END IF
 50               CONTINUE
               END IF
C                                       move on to next scan
            CURSCN = CURSCN + 1
            I1 = MAXANT * MAXANT
            CALL DFILL (I1, 0.0D0, VISCNT)
            GO TO 10
            END IF
C                                       count good samples
         DO 80 JIF = 1,NIF
            DO 70 JF = 1,NF
               DO 60 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  IF (VIS(3,INDEXI).GT.0.0) VISCNT(IA1,IA2) =
     *               VISCNT(IA1,IA2) + 1.0D0
 60               CONTINUE
 70            CONTINUE
 80         CONTINUE
C                                       last call - no vis
      ELSE
C                                       squirrel away current one
         IF (CURSCN.GT.0) THEN
            DO 140 I2 = 1,NSTNS
               DO 130 I1 = 1,NSTNS
                  IF ((VISCNT(I1,I2).GT.0.0D0) .AND.
     *               (ISANTS(I1,CURSCN).GE.0) .AND.
     *               (ISANTS(I2,CURSCN).GE.0)) THEN
                     NOBLNS(CURSCN) = NOBLNS(CURSCN) + 1
                     ISANTS(I1,CURSCN) = ISANTS(I1,CURSCN) +
     *                  VISCNT(I1,I2)
                     ISANTS(I2,CURSCN) = ISANTS(I2,CURSCN) +
     *                  VISCNT(I1,I2)
                     END IF
 130              CONTINUE
 140           CONTINUE
            ISANTM(CURSCN) = 0.0D0
            DO 150 I1 = 1,NSTNS
               IF (ISANTS(I1,CURSCN).GT.0.0D0) THEN
                  NOANTS(CURSCN) = NOANTS(CURSCN) + 1
                  ISANTM(CURSCN) = ISANTM(CURSCN) + ISANTS(I1,CURSCN)
                  END IF
 150           CONTINUE
            END IF
C                                       look over results: all ants?
         CALL RFILL (30, 0.0, CHNSEL)
         CALL RFILL (10, 0.0, APARM)
         JIF = 0
         JF = 0
         JS = 0
         DO 200 I1 = 1,NOSCAN
            IF (NOANTS(I1).GE.NOSTNS) THEN
               JS = JS + 1
               IF (NOBLNS(I1).GT.JF) THEN
                  JF = NOBLNS(I1)
                  JIF = I1
                  END IF
               END IF
 200        CONTINUE
C                                       is there one only?
         IF (JS.EQ.1) THEN
            CHNSEL(1,1) = TSCAN(1,JIF)
            CHNSEL(2,1) = TSCAN(2,JIF)
            CHNSEL(3,1) = NOANTS(JIF)
C                                       more than one - pick
         ELSE IF (JS.GT.1) THEN
            I2 = 0
 210        IF ((I2.LT.JS) .AND. (I2.LT.10)) THEN
               JIF = 0
               DTEMP = 0.0D0
               DO 215 I1 = 1,NOSCAN
                  IF ((NOANTS(I1).EQ.NOSTNS) .AND. (NOBLNS(I1).EQ.JF))
     *               THEN
                     IF (ISANTM(I1).GT.DTEMP) THEN
                        DTEMP = ISANTM(I1)
                        JIF = I1
                     ELSE IF ((ISANTM(I1).EQ.DTEMP) .AND.
     *                  (PCOUNT(I1).GT.PCOUNT(JIF))) THEN
                        DTEMP = ISANTM(I1)
                        JIF = I1
                        END IF
                     END IF
 215              CONTINUE
               IF (JIF.GT.0) THEN
                  I2 = I2 + 1
                  CHNSEL(1,I2) = TSCAN(1,JIF)
                  CHNSEL(2,I2) = TSCAN(2,JIF)
                  CHNSEL(3,I2) = NOANTS(JIF)
                  NOBLNS(JIF) = 1000000
                  GO TO 210
                  END IF
               END IF
C                                       use up rest
 220        IF ((I2.LT.JS) .AND. (I2.LT.10)) THEN
               JIF = 0
               DTEMP = 0.0D0
               DO 225 I1 = 1,NOSCAN
                  IF ((NOANTS(I1).EQ.NOSTNS) .AND. (NOBLNS(I1).LT.JF))
     *               THEN
                     IF (ISANTM(I1).GT.DTEMP) THEN
                        DTEMP = ISANTM(I1)
                        JIF = I1
                        END IF
                     ELSE IF ((ISANTM(I1).EQ.DTEMP) .AND.
     *                  (PCOUNT(I1).GT.PCOUNT(JIF))) THEN
                        DTEMP = ISANTM(I1)
                        JIF = I1
                     END IF
 225              CONTINUE
               IF (JIF.GT.0) THEN
                  I2 = I2 + 1
                  CHNSEL(1,I2) = TSCAN(1,JIF)
                  CHNSEL(2,I2) = TSCAN(2,JIF)
                  CHNSEL(3,I2) = NOANTS(JIF)
                  NOBLNS(JIF) = 1000000
                  GO TO 220
                  END IF
               END IF
C                                       none have all (damn)
         ELSE
            NHOPE = NOSTNS - 1
            JIF = 0
            JF = 0
            JS = 0
            DO 300 I1 = 1,NOSCAN
               IF (NOANTS(I1).GE.NHOPE) THEN
                  JS = JS + 1
                  IF (NOBLNS(I1).GT.JF) THEN
                     JF = NOBLNS(I1)
                     JIF = I1
                     END IF
                  END IF
 300           CONTINUE
C                                       more than 1 - which best
            IF (JS.GT.1) THEN
               IC = 0
               JIF = 0
               DTEMP = 0.0D0
               DO 310 I1 = 1,NOSCAN
                  IF ((NOANTS(I1).GE.NHOPE) .AND. (NOBLNS(I1).EQ.JF))
     *               THEN
                     IC = IC + 1
                     IF (ISANTM(I1).GT.DTEMP) THEN
                        DTEMP = ISANTM(I1)
                        JIF = I1
                     ELSE IF ((ISANTM(I1).EQ.DTEMP) .AND.
     *                  (PCOUNT(I1).GT.PCOUNT(JIF))) THEN
                        DTEMP = ISANTM(I1)
                        JIF = I1
                        END IF
                     END IF
 310              CONTINUE
               END IF
            IF (JIF.GT.0) THEN
               CHNSEL(1,1) = TSCAN(1,JIF)
               CHNSEL(2,1) = TSCAN(2,JIF)
               CHNSEL(3,1) = NOANTS(JIF)
C                                       which antenna missing
               I2 = 0
               DO 320 I1 = 1,NSTNS
                  IF (ISANTS(I1,JIF).EQ.0.0D0) I2 = I1
 320              CONTINUE
               APARM(1) = I2
C                                       find scan w that antenna
               JIF = 0
               JF = 0
               JS = 0
               DO 330 I1 = 1,NOSCAN
                  IF (NOANTS(I1).GE.2) THEN
                     JS = JS + 1
                     IF ((NOBLNS(I1).GT.JF) .AND.
     *                  (ISANTS(I2,I1).GT.0.0D0)) THEN
                        JF = NOBLNS(I1)
                        JIF = I1
                        END IF
                     END IF
 330              CONTINUE
               IF (JIF.GT.0) THEN
                  CHNSEL(1,2) = TSCAN(1,JIF)
                  CHNSEL(2,2) = TSCAN(2,JIF)
                  CHNSEL(3,2) = NOANTS(JIF)
                  END IF
               END IF
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE GSCANS (LUN, DISK, CNO, CATBLK, ISUB, ISRC, BUFFER,
     *   NOSCAN, TSCAN)
C-----------------------------------------------------------------------
C   GSCANS reads the NX table and makes a list of scan boundaries
C   Inputs:
C      LUN      I        LUN to use
C      DISK     I        Disk number
C      CNO      I        Catalog number
C      CATBLK   I(*)     Header
C      ISUB     I        Limit to subarray ISUB - 0 -> all
C      ISRC     I        Source number
C   Outputs
C      BUFFER   I(*)     Scratch buffer
C      NOSCAN   I        Number of times in TSCAN
C      TSCAN    R(2,*)   Start/stop times of scan boundaries
C-----------------------------------------------------------------------
      INTEGER   LUN, DISK, CNO, CATBLK(256), ISUB, ISRC, BUFFER(*),
     *   NOSCAN
      REAL      TSCAN(2,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, INXRNO, NXKOLS(MAXNXC), NXNUMV(MAXNXC), IERR,
     *   IDSOUR, SUBARR, VSTART, VEND, FREQID, NROW, IROW
      REAL      TIME, DTIME
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      VER = 1
      CALL NDXINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, INXRNO,
     *   NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING INDEX TABLE'
         GO TO 900
         END IF
      NROW = BUFFER(5)
      NOSCAN = 0
      DO 100 IROW = 1,NROW
         CALL TABNDX ('READ', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING INDEX TABLE'
            GO TO 900
            END IF
         IF (ISRC.EQ.IDSOUR) THEN
            IF ((ISUB.LE.0) .OR. (SUBARR.LE.0) .OR. (ISUB.EQ.SUBARR))
     *         THEN
               NOSCAN = NOSCAN + 1
               TSCAN(1,NOSCAN) = TIME - 0.5 * DTIME
               TSCAN(2,NOSCAN) = TIME + 0.5 * DTIME
               END IF
            END IF
 100     CONTINUE
C
 900  IF (IERR.NE.0) THEN
         CALL MSGWRT (6)
         NOSCAN = 0
         END IF
      CALL TABNDX ('CLOS', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME, DTIME,
     *   IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GSCANS ERROR',I4,' ON ',A)
      END
      SUBROUTINE GETPCS (LUN, DISK, CNO, CATBLK, ISUB, ISRC, BUFFER,
     *   NOSCAN, TSCAN, PCVERS, PCOUNT)
C-----------------------------------------------------------------------
C   GETPCS counts PC table records by scan
C   Inputs:
C      LUN      I        LUN to use
C      DISK     I        Disk number
C      CNO      I        Catalog number
C      CATBLK   I(*)     Header
C      ISUB     I        Limit to subarray ISUB - 0 -> all
C      ISRC     I        Source number
C      NOSCAN   I        Number of times in TSCAN
C      TSCAN    R(2,*)   Start/stop times of scan boundaries
C      PCVERS   I        PC table version
C   Outputs
C      BUFFER   I(*)     Scratch buffer
C      PCOUNT   I(*)     Count of PC records in each scan
C-----------------------------------------------------------------------
      INTEGER   LUN, DISK, CNO, CATBLK(256), ISUB, ISRC, BUFFER(*),
     *   NOSCAN, PCVERS, PCOUNT(*)
      REAL      TSCAN(2,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PPCV.INC'
      INTEGER   PCROW, PCKOLS(MAXPCC), PCNUMV(MAXPCC), NUMPOL, NUMIF,
     *   NUMTON, IRET, NROW, IROW, SOURCE, SUBARR, ANTNUM, FREQID, J
      DOUBLE PRECISION TIME, CABCAL, PCFREQ(2,MAXTON,MAXIF)
      REAL      TIMINT, STATE(2,4,MAXIF), PCREAL(2,MAXTON,MAXIF),
     *   PCIMAG(2,MAXTON,MAXIF), PCRATE(2,MAXTON,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       open
      CALL PCINI ('READ', BUFFER, DISK, CNO, PCVERS, CATBLK, LUN,
     *   PCROW, PCKOLS, PCNUMV, NUMPOL, NUMIF, NUMTON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING PC TABLE'
         GO TO 990
         END IF
      NROW = BUFFER(5)
      DO 100 IROW = 1,NROW
         PCROW = IROW
         CALL TABPC ('READ', BUFFER, PCROW, PCKOLS, PCNUMV, NUMPOL,
     *      TIME, TIMINT, SOURCE, ANTNUM, SUBARR, FREQID, CABCAL, STATE,
     *      PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING PC TABLE'
            GO TO 990
            END IF
C                                       interested in this record
         IF (((ISUB.LE.0) .OR. (SUBARR.LE.0) .OR. (ISUB.EQ.SUBARR))
     *     .AND. ((ISRC.LE.0) .OR. (SOURCE.LE.0) .OR.
     *      (SOURCE.EQ.ISRC))) THEN
            DO 20 J = 1,NOSCAN
               IF ((TIME.GE.TSCAN(1,J)) .AND. (TIME.LE.TSCAN(2,J))) THEN
                  PCOUNT(J) = PCOUNT(J) + 1
                  GO TO 100
                  END IF
 20            CONTINUE
            END IF
 100     CONTINUE
C                                       close
      CALL TABPC ('CLOS', BUFFER, PCROW, PCKOLS, PCNUMV, NUMPOL, TIME,
     *   TIMINT, SOURCE, ANTNUM, SUBARR, FREQID, CABCAL, STATE, PCFREQ,
     *   PCREAL, PCIMAG, PCRATE, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETPCS: ERROR:',I4,' ON ',A)
      END

