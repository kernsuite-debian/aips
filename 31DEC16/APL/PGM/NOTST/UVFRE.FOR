LOCAL INCLUDE 'UVFRE.INC'
C                                       Local include for UVFRE
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XCLAI2(2), XSOUR(4,30),
     *   XCALC, XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XSEQ2, XDISK2, XQUAL, XTIME(8), XBAND,
     *   XFREQ, XFQID, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT, XDISO, XINTP(3)
      REAL      REFPXO, REFPXI, SCRBUF(256), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQ2, SEQOUT, DISKIN, DISK2, DISKO, JBUFSZ,
     *   ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECO, NRPRMI, NRPRMO, OLDCNO, CNO2, NEWCNO,
     *   CAT2(256), CATOUT(256), NUMIFO, NCHANO, NUMIFI, NCHANI, NFRX
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, NAMOUT*12,
     *   CLAOUT*6
      DOUBLE PRECISION REFRQI, REFRQO, FOFFO(MAXIF), FINCO(MAXIF),
     *   FRQO(2,MAXIF)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAME2, XCLAI2,
     *   XSEQ2, XDISK2, XSOUR, XQUAL, XCALC,XTIME, XBAND, XFREQ, XFQID,
     *   XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, XDOAC, XINTP, XNAMOU, XCLAOU, XSOUT, XDISO
      COMMON /UVFREP/ CATOLD, CATOUT, CAT2, FOFFO, FINCO, REFRQI,
     *   REFRQO, FRQO, REFPXI, REFPXO, SEQIN, SEQ2, SEQOUT, DISKIN,
     *   DISK2, DISKO, ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECO, NRPRMI, NRPRMO, ISCOMP, OLDCNO, NEWCNO, CNO2,
     *   NUMIFI, NUMIFO, NFRX, NCHANI, NCHANO
      COMMON /CHARPM/ NAMEIN, CLAIN, NAME2, CLAS2, NAMOUT, CLAOUT
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       End local include for UVFRE
LOCAL END
      PROGRAM UVFRE
C-----------------------------------------------------------------------
C! Replaces all IFs with a single spectrum
C# Utility UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2015
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
C   UVFRE replaces all IFs with a single UV spectrum, averaging
C   overlapped channels and flagging missing ones.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C   full set of calibration adverbs
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, N, NWORDS, IHSTRT(2)
      REAL      WEIGHT(2), CHSTRT(2)
      LONGINT   PWGHT, PCHST
      EQUIVALENCE (IHSTRT, CHSTRT)
      INCLUDE 'UVFRE.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVFRE '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVFRIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       dynamic memory
      N = CATBLK(KINAX+JLOCF) * CATBLK(KINAX+JLOCIF)
      NWORDS = (N - 1) / 1024 + 2
      CALL ZMEMRY ('GET ', PRGM, NWORDS, CHSTRT, PCHST, IRET)
      IF (IRET.EQ.0) THEN
         NWORDS = (N * NFRX - 1) / 1024 + 4
         CALL ZMEMRY ('GET ', PRGM, NWORDS, WEIGHT, PWGHT, IRET)
         END IF
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET NEEDED DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       get weight arrays
      CALL UVFRWT (N, IHSTRT(1+PCHST), WEIGHT(1+PWGHT), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL UVFRUV (N, IHSTRT(1+PCHST), WEIGHT(1+PWGHT), IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVFRHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE UVFRIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   UVFRIN gets input parameters for UVFRE and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               5 => catalog troubles
C                               8 => can't start
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
      INTEGER   IRET
      CHARACTER PRGN*6
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, LUN, VER, LUNTMP, FREQID
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20), DEF1(5), DEF2(5)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'UVFRE.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCHND.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK  /' '/
      DATA DEF1 /4., 2., 2., 3., 1.99/
      DATA DEF2 /1., 3., 1., 4., 1./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 170
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAI2, CLAS2)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      STOKES = ' '
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      SEQIN = IROUND (XSIN)
      SEQ2 = IROUND (XSEQ2)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISK2 = IROUND (XDISK2)
      DISKO = IROUND (XDISO)
C                                       interp function
      I = IROUND (XINTP(1))
      IF ((I.LT.0) .OR. (I.GT.5)) I = 0
      XINTP(1) = I
      IF (XINTP(2).LT.0.1) XINTP(2) = DEF1(I)
      IF (XINTP(3).LT.XINTP(2)) XINTP(3) = DEF2(I) * XINTP(2)
C                                       get master freq info
C                                       Get CATBLK from old file.
      CNO2 = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK2, CNO2, NAME2, CLAS2, SEQ2, PTYPE,
     *   NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAME2, CLAS2, SEQ2, DISK2, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK2, CNO2, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING MASTER DATA SET HEADER'
         GO TO 990
         END IF
      CALL UVPGET (IERR)
      REFRQO = CATD(KDCRV+JLOCF)
      NCHANO = CATBLK(KINAX+JLOCF)
      REFPXO = CATR(KRCRP+JLOCF)
      CALL COPY (256, CATBLK, CAT2)
C                                       get FQ settings first
      LUN = LUNTMP (1)
      VER = 1
      CALL CHNDAT ('READ', BUFF2, DISK2, CNO2, VER, CATBLK, LUN,
     *   NUMIFO, FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE FOR VELOCITIES'
         GO TO 990
         END IF
      DO 15 I = 1,NUMIFO
         FOFFO(I) = FOFF(I) + REFRQO
         FINCO(I) = FINC(I)
         FRQO(1,I) = FOFFO(I) + FINCO(I) * (0.5 - REFPXO)
         FRQO(2,I) = FOFFO(I) + FINCO(I) * (NCHANO+0.5 - REFPXO)
 15      CONTINUE
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = XDOAC.GT.0.0
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
         WRITE (MSGTXT,1000) IERR, 'GETTING INPUT DATA SET HEADER'
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = 1
         EIF = CATBLK(KINAX+JLOCIF)
         END IF
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
      REFRQI = CATD(KDCRV+JLOCF)
      NCHANI = ECHAN
      REFPXI = CATR(KRCRP+JLOCF)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       get freq information
      VER = 1
      CALL CHNDAT ('READ', BUFF2, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *   NUMIFI, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) GO TO 999
      CATD(KDCRV+JLOCF) = REFRQO
      CATR(KRCRP+JLOCF) = REFPXI
      CATBLK(KINAX+JLOCF) = NCHANO
      CATBLK(KINAX+JLOCIF) = NUMIFO
      CALL COPY (256, CATBLK, CATOUT)
      DO 20 I = 1,NUMIFI
         FOFF(I) = FOFF(I) + REFRQI
 20      CONTINUE
      IF (XINTP(1).EQ.0.0) THEN
         NFRX = ABS (FINC(1) / FINCO(1)) + 2.0D0
      ELSE
         NFRX = XINTP(3) * ABS (FINC(1) / FINCO(1)) + 2.0D0
         END IF
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1035) IRET
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       replace with our header
      CALL COPY (256, CATOUT, CATBLK)
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
      IRET = 4
      CALL UVCREA (DISKO, CCNO, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
         ELSE
            WRITE (MSGTXT,1060)
            END IF
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
      NEWCNO = CCNO
C                                       Save output file info
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      CALL COPY (256, CATBLK, CATOUT)
      CALL COPY (256, CATOLD, CATBLK)
      CALL UVPGET (IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVFRIN: ERROR',I3,' ON ',A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY NOT OVERWRITE EXISTING FILE.  QUITTING')
      END
      SUBROUTINE UVFRWT (N, CHSTRT, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   UBGRWT determines the weight of the convolution of each input
C   channel to a number of output channels
C   Inputs;
C      N        I      number input channels
C   Outputs:
C      CHSTRT   I(*)   initial output channel for each input channel
C      WEIGHT   R(*)   weights of each input channel to outputs channels
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   N, CHSTRT(*), IRET
      REAL      WEIGHT(N,*)
C
      INCLUDE 'UVFRE.INC'
      INTEGER   I, J, IIF, ICH, ITYPE, J1, J2, K, L
      DOUBLE PRECISION FF, FI1, FI2, FO1, FO2, DOS, DOF, DP
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DCHND.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ITYPE = XINTP(1) + 0.01
      CALL FILL (N, 0, CHSTRT)
      I = N * NFRX
      CALL RFILL (I, 0.0, WEIGHT)
      I = NUMIFO * NCHANO
      CALL RFILL (I, 0.0, BUFF2)
C                                       simplistic
      IF (ITYPE.EQ.0) THEN
         DO 25 I = 1,N
            IIF = (I - 1) / NCHANI + 1
            ICH = I - (IIF - 1) * NCHANI
            FI1 =  FOFF(IIF) + FINC(IIF) * (ICH - 0.5D0 - REFPXI)
            FI2 =  FI1 + FINC(IIF)
            J1 = 0
            J2 = 0
C                                       find output IF(s)
            DO 10 J = 1,NUMIFO
               IF ((FI1.GE.FRQO(1,J)) .AND. (FI1.LE.FRQO(2,J))) J1 = J
               IF ((FI2.GE.FRQO(1,J)) .AND. (FI2.LE.FRQO(2,J))) J2 = J
 10            CONTINUE
            IF ((J2.GT.0) .AND. (J1.EQ.0)) J1 = 1
            IF ((J1.GT.0) .AND. (J2.EQ.0)) J2 = NUMIFO
            L = 0
            IF ((J1.GT.0) .AND. (J2.GT.0)) THEN
               DO 20 J = J1,J2
                  DO 15 K = 1,NCHANO
                     FO1 = FOFFO(J) + (K - 0.5D0 - REFPXO) * FINCO(J)
                     FO2 = FO1 + FINCO(J)
                     IF (FI2.LE.FO1) THEN
                        GO TO 20
                     ELSE IF (FI1.LT.FO2) THEN
                        L = L + 1
                        IF (L.EQ.1) CHSTRT(I) = (J-1) * NCHANO + K
                        IF (FI1.GT.FO1) THEN
                           WEIGHT(I,L) = (FI1 - FO1) / (FO2 - FO1)
                        ELSE IF (FI2.LT.FO2) THEN
                           WEIGHT(I,L) = (FI2 - FO1) / (FO2 - FO1)
                        ELSE
                           WEIGHT(I,L) = 1.0
                           END IF
                        END IF
 15                  CONTINUE
 20               CONTINUE
               END IF
 25         CONTINUE
C                                       convolution
      ELSE
         DO 75 I = 1,N
            IIF = (I - 1) / NCHANI + 1
            ICH = I - (IIF - 1) * NCHANI
            FF =  FOFF(IIF) + FINC(IIF) * (ICH - REFPXI)
            DOS = XINTP(3) * FINC(IIF) / 2.0D0
            DOF = XINTP(2) * FINC(IIF) / 2.0D0
C                                       parm
            IF (ITYPE.EQ.2) THEN
               DP = -LOG (2.0D0) / (DOF * DOF)
            ELSE IF (ITYPE.EQ.4) THEN
               DP = PI / (2.0D0 * DOF)
            ELSE IF (ITYPE.EQ.5) THEN
               DP = -LOG (2.0D0) / DOF
               END IF
            FI1 = FF - DOS
            FI2 = FF + DOS
            J1 = 0
            J2 = 0
C                                       find output IF(s)
            DO 40 J = 1,NUMIFO
               IF ((FI1.GE.FRQO(1,J)) .AND. (FI1.LE.FRQO(2,J))) J1 = J
               IF ((FI2.GE.FRQO(1,J)) .AND. (FI2.LE.FRQO(2,J))) J2 = J
 40            CONTINUE
            IF ((J2.GT.0) .AND. (J1.EQ.0)) J1 = 1
            IF ((J1.GT.0) .AND. (J2.EQ.0)) J2 = NUMIFO
            L = 0
            IF ((J1.GT.0) .AND. (J2.GT.0)) THEN
               DO 70 J = J1,J2
                  DO 65 K = 1,NCHANO
                     FO1 = FOFFO(J) + (K - REFPXO) * FINCO(J)
                     IF (FO1.GT.FI2) THEN
                        GO TO 70
                     ELSE IF ((FO1.GE.FI1) .AND. (FO2.LE.FI2)) THEN
                        L = L + 1
                        IF (L.EQ.1) CHSTRT(I) = (J-1)*NCHANO + K
C                                       Hanning
                        IF (ITYPE.EQ.1) THEN
                           WEIGHT(I,L) = 1.0 - ABS (FO1-FF) / DOF
                           WEIGHT(I,L) = MAX (0.0, WEIGHT(I,L))
C                                       Gaussian
                        ELSE IF (ITYPE.EQ.2) THEN
                           WEIGHT(I,L) = EXP (DP * ((FO1-FF)**2))
C                                       Boxcar
                        ELSE IF (ITYPE.EQ.3) THEN
                           IF (ABS(FO1-FF).LE.DOF) WEIGHT(I,L) = 1.0
C                                       Sinc
                        ELSE IF (ITYPE.EQ.4) THEN
                           IF (FO1.EQ.FF) THEN
                              WEIGHT(I,L) = 1.0
                           ELSE
                              WEIGHT(I,L) = SIN (DP * (FO1-FF)) /
     *                           (DP * (FO1-FF))
                              END IF
C                                       exponential
                        ELSE IF (ITYPE.EQ.5) THEN
                           WEIGHT(I,L) = EXP (DP * ABS(FO1-FF))
                           END IF
                        END IF
 65                  CONTINUE
 70               CONTINUE
               END IF
 75         CONTINUE
         END IF
C                                       normalize: sum
      DO 120 I = 1,N
         IF (CHSTRT(I).GT.0) THEN
            J1 = CHSTRT(I)
            J2 = J1 + NFRX - 1
            L = 0
            DO 110 J = J1,J2
               L = L + 1
               BUFF2(J) = BUFF2(J) + WEIGHT(I,L)
 110           CONTINUE
            END IF
 120     CONTINUE
C                                       normalize: divide
      DO 140 I = 1,N
         IF (CHSTRT(I).GT.0) THEN
            J1 = CHSTRT(I)
            J2 = J1 + NFRX - 1
            L = 0
            DO 130 J = J1,J2
               L = L + 1
               IF (BUFF2(J).GT.0.0) WEIGHT(I,L) = WEIGHT(I,L) / BUFF2(J)
 130           CONTINUE
            END IF
 140     CONTINUE
C                                       count
      L = NUMIFO * NCHANO
      DP = 0.0D0
      DO 150 I = 1,L
         IF (BUFF2(I).GT.0) DP = DP + 1.0D0
 150     CONTINUE
      IF (DP.LE.0.0D0) THEN
         IRET = 8
         MSGTXT = 'NO CHANNELS WILL BE WRITTEN'
         CALL MSGWRT (8)
      ELSE
         IRET = 0
         DP = 1.D2 * DP / L
         WRITE (MSGTXT,1150) DP
         CALL MSGWRT (3)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT (F8.3,' per cent of the output channels will contain data')
      END
      SUBROUTINE UVFRUV (N, CHSTRT, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   UVFRUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Inputs:
C      N        I      number input channels
C      CHSTRT   I(*)   initial output channel for each input channel
C      WEIGHT   R(*)   weights of each input channel to outputs channels
C   Input in common:
C      NRPRMI   I      Input number of random parameters.
C      INCSI    I      Input Stokes' increment in vis.
C      INCFI    I      Input frequency increment in vis.
C      INCIFI   I      Input IF increment in vis.
C      LRECO    I      Output file record length
C      NRPRMO   I      Output number of random parameters.
C      INCSO    I      Output Stokes' increment in vis.
C      INCFO    I      Output frequency increment in vis.
C      INCIFO   I      Output IF increment in vis.
C      ISCOMP   L      If true data is compressed
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   N, CHSTRT(*), IRET
      REAL      WEIGHT(N,*)
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, BO, VO,
     *   NUMVIS, XCOUNT, NCORO, NCOPY, RNXRET
      LOGICAL   T, F
      INCLUDE 'UVFRE.INC'
      REAL      VIS(UVBFSS), RESULT(UVBFSS), RPARM(20)
      DOUBLE PRECISION UVSCAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN-FOR-WRITE VIS FILE'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT-FOR-WRITE VIS FILE'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NUMVIS = 0
      XCOUNT = 0
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, NEWCNO, CATOUT, RNXRET)
      UVSCAL = REFRQO / REFRQI
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING VIS FILE'
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
C                                       call user routine
         CALL UVFREO (N, CHSTRT, WEIGHT, VIS, RESULT)
C                                       Copy to output.

         XCOUNT = XCOUNT + 1.0D0
         CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
C                                       update NX table
         CALL RNXUPD (RPARM, RNXRET)
C                                       Compressed
         IF (ISCOMP) THEN
            CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *         BUFF2(IPTRO+NRPRMO))
         ELSE
            CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
            END IF
         IPTRO = IPTRO + LRECO
         NIOUT = NIOUT + 1
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING VIS FILE'
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING VIS FILE'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATOUT, IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       close NX table
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVFRUV: ERROR',I3,' ON ',A)
      END
      SUBROUTINE UVFRHI
C-----------------------------------------------------------------------
C   UVFRHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, WHAT(6)*12
      INTEGER   LUN1, LUN2, IERR, I
      INCLUDE 'UVFRE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA WHAT /'Simple', 'Hanning', 'Gaussian', 'Boxcar', 'Sinc',
     *   'Exponential'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATOUT,
     *   SCRBUF, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       INTPARM
      I = XINTP(1) + 0.1
      WRITE (HILINE,1011) TSKNAM, I, WHAT(I+1)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (I.GT.0) THEN
         WRITE (HILINE,1012) TSKNAM, XINTP(2)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1013) TSKNAM, XINTP(3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables: local version
      CALL COPTAB (DISKIN, OLDCNO, DISK2, CNO2, DISKO, NEWCNO, CATOUT,
     *   IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'UVFRHI: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATOUT, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVFRHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1011 FORMAT (A,'INTPARM(1)=',I2,10X,'/ function: ',A)
 1012 FORMAT (A,'INTPARM(2)=',F6.2,6X,'/ function width')
 1013 FORMAT (A,'INTPARM(3)=',F6.2,6X,'/ function support')
      END
      SUBROUTINE UVFREO (N, CHSTRT, WEIGHT, VIS, RESULT)
C-----------------------------------------------------------------------
C   This does the spectral building
C   Inputs:
C      N        I      number input channels
C      CHSTRT   I(*)   initial output channel for each input channel
C      WEIGHT   R(*)   weights of each input channel to outputs channels
C      VIS      R(3,*) Visibilities in order real, imaginary, weight
C                      (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      CATBLK   I(256) Catalog header record
C      INCSI    I      Input Stokes' increment in vis.
C      INCFI    I      Input frequency increment in vis.
C      INCIFI   I      Input IF increment in vis.
C   Output:
C      RESULT   R(3,*) Output visibilities gridded in frequency.
C-----------------------------------------------------------------------
      INTEGER   N, CHSTRT(*)
      REAL      WEIGHT(N,*), VIS(3,*), RESULT(3,*)
C
      INCLUDE 'UVFRE.INC'
      INTEGER   JIF, JF, JS, NS, INDEXO, INDEXI, NT, KF, KIF, K1, K2, I,
     *   K
      REAL      W, WT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       pointers to traverse the data
      NS = 1
      IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
      NT = NS * NCHANO * NUMIFO
      CALL RFILL (3*NT, 0.0, RESULT)
      DO 40 JS = 1,NS
         I = 0
         DO 30 JIF = 1,NUMIFI
            DO 20 JF = 1,NCHANI
               I = I + 1
               INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *            (JS-1) * INCSI + 1
               W = VIS(3,INDEXI)
               IF (W.GT.0.0) THEN
                  K1 = CHSTRT(I)
                  K2 = K1 + NFRX - 1
                  DO 10 K = K1,K2
                     KIF = (K - 1) / NCHANO + 1
                     KF = K - (KIF - 1) * NCHANO
                     INDEXO = JS + (KIF - 1) * INCIFO + (KF - 1) * INCFO
                     WT = WEIGHT(I,K-K1+1) * W
                     RESULT(1,INDEXO) = RESULT(1,INDEXO) +
     *                  WT * VIS(1,INDEXI)
                     RESULT(2,INDEXO) = RESULT(2,INDEXO) +
     *                  WT * VIS(2,INDEXI)
                     RESULT(3,INDEXO) = RESULT(3,INDEXO) + WT
 10                  CONTINUE
                  END IF
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
      DO 50 INDEXO = 1,NT
         W = RESULT(3,INDEXO)
         IF (W.GT.0) THEN
            RESULT(1,INDEXO) = RESULT(1,INDEXO) / W
            RESULT(2,INDEXO) = RESULT(2,INDEXO) / W
            END IF
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE COPTAB (DISKIN, CNOIN, DISK2, CNO2, DISKOU, CNOOUT,
     *   CATO, IRET)
C-----------------------------------------------------------------------
C   Updates tables for selection by IF etc
C   UVFRE version changes a lot!
C   Inputs:
C      DISKIN   I   Input disk number
C      CNOIN    I   Input catalog number
C      DISKOU   I   Output disk number
C      CNOOUT   I   Output catalog number
C   Inputs in common:
C      BIF   I  First IF
C      EIF   I  Highest IF selected
C      FQOFF D  Frequency offset
C   Output:
C      IRET  I  Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, DISK2, CNO2, DISKOU, CNOOUT, CATO(256),
     *   IRET
C
      INCLUDE 'INCS:DSEL.INC'
      REAL      CATOR(256)
      LOGICAL   TABLE, EXIST, FITASC, MULTI
      CHARACTER NOTTYP(20)*2
      INTEGER   IERR, LUN1, LUN2, VER, NVER, OFQID, ISUB, JSUB, AN(50),
     *   NA, BUFF1(512), BUFF2(512),
     *   NONOT, EIFSAV, CATOUT(256), BPOL, EPOL
      DOUBLE PRECISION TIME1, TIME2, FQOFF, CATOD(128)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATOUT, CATOR, CATOD)
      DATA LUN1, LUN2 /28, 29/
      DATA NONOT, NOTTYP /20, 'NX','FQ','CH','CL','SN','SU','FG','BP',
     *   'IM','CQ','PC','TY','GC','MC','WX','BL','AN','CP','PD','SY'/
C-----------------------------------------------------------------------
C                                       Single source now?
      CALL COPY (256, CATO, CATOUT)
      CALL COPY (256, CATO, CATBLK)
      CALL UVPGET (IRET)
      MULTI = ILOCSU.GT.0
      EIF = CATBLK(KINAX+JLOCIF)
      BIF = 1
      EIFSAV = EIF
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKOU, CNOIN,
     *   CNOOUT, CATOUT, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'FLGIHI: ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
C                                       more complex tables
      MSGTXT = 'Updating tables for IF/FREQID/channel selection'
      CALL MSGWRT (4)
      ISUB = 0
      JSUB = -1
      NA = 0
C                                       STOKES selection not allowed
C                                       these ok even for 1 pol data
      BPOL = 1
      EPOL = 2
C                                       allow 15 min extra to be sure
C                                       that one gets all needed rows
      TIME1 = TSTART - 0.0104D0
      TIME2 = TEND + 0.0104D0
      CALL FILL (50, 0, AN)
C                                       FQ table
      IF (JLOCIF.GT.0) THEN
         VER = 1
         CALL TABCOP ('FQ', VER, VER, LUN1, LUN2, DISK2, DISKOU, CNO2,
     *      CNOOUT, CATOUT, BUFF1, BUFF2, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Reference frequency in AN table
C                                       IF selection
      CALL FNDEXT ('AN', CATUV, NVER)
      DO 100 VER = 1,NVER
         CALL ISTAB ('AN', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((EXIST) .AND. (IERR.EQ.0)) CALL ANSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, VER, CATUV, CATOUT, LUN1, LUN2, BIF,
     *      EIF, FQOFF, DOPOL, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 100     CONTINUE
C                                       Revise tables:
      OFQID = FRQSEL
C                                       CL tables
      CALL FNDEXT ('CL', CATUV, NVER)
      IF (.NOT.MULTI) NVER = 0
C                                       write null table
      VER = 1
      CALL ISTAB ('CL', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *   EXIST, FITASC, IERR)
C                                       do NOT select on sources
      IF (EXIST .AND. (IERR.EQ.0)) THEN
         IF (MULTI) THEN
            CALL CLNULL (DISKIN, CNOIN, DISKOU, CNOOUT, VER, CATUV,
     *         CATOUT, LUN1, LUN2, BPOL, EPOL, BIF, EIF, OFQID, TIME1,
     *         TIME2, 0, SOUWAN, AN, NA, ISUB, JSUB, BUFF1, BUFF2, IRET)
         ELSE
            CALL CL2FO (DISKIN, CNOIN, VER, LUN1, CATUV, DISKOU, CNOOUT,
     *         LUN2, CATOUT, SOUWAN, BUFF1, BUFF2, IRET)
            END IF
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       CP tables: none
      CALL FNDEXT ('CP', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'CP TABLES NOT COPIED'
         CALL MSGWRT (6)
         END IF
C                                       CQ tables
      CALL FNDEXT ('CQ', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'CQ TABLES NOT COPIED'
         CALL MSGWRT (6)
         END IF
C                                       FG tables
      CALL FNDEXT ('FG', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'FLAG TABLES NOT COPIED'
         CALL MSGWRT (6)
         END IF
C                                       GC tables
      CALL FNDEXT ('GC', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'GC TABLES NOT COPIED'
         CALL MSGWRT (6)
         END IF
C                                       IM tables
      CALL FNDEXT ('IM', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'IM TABLES NOT COPIED'
         CALL MSGWRT (6)
         END IF
C                                       MC tables
      CALL FNDEXT ('MC', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'MC TABLES NOT COPIED'
         CALL MSGWRT (6)
         END IF
C                                       PC tables
      CALL FNDEXT ('PC', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'PC TABLES NOT COPIED'
         CALL MSGWRT (6)
         END IF
C                                       SU tables
      IF (ILOCSU.GE.0) THEN
         CALL FNDEXT ('SU', CATUV, NVER)
         DO 300 VER = 1,NVER
            CALL ISTAB ('SU', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
            IF (EXIST .AND. (IERR.EQ.0)) CALL SUSEL (DISKIN, CNOIN,
     *         DISKOU, CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BIF, EIF,
     *         OFQID, BUFF1, BUFF2, IRET)
            IF (IRET.GT.0) GO TO 999
 300        CONTINUE
         END IF
C                                       SY tables
      CALL FNDEXT ('SY', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'SY TABLES NOT COPIED'
         CALL MSGWRT (6)
         END IF
C                                       TY tables
      CALL FNDEXT ('TY', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'TY TABLES NOT COPIED'
         CALL MSGWRT (6)
         END IF
C                                       WX tables
      CALL FNDEXT ('WX', CATUV, NVER)
      DO 340 VER = 1,NVER
         CALL ISTAB ('WX', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL WXSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, TIME1,
     *      TIME2, AN, NA, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 340     CONTINUE
C
      EIF = EIFSAV
      CALL COPY (256, CATOUT, CATO)
C
 999  RETURN
      END
