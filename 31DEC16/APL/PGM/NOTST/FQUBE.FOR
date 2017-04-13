LOCAL INCLUDE 'FQUBE.INC'
      INCLUDE 'INCS:PMAD.INC'
      REAL      INSEQ, INDSK, IN2SEQ, IN3SEQ, OUTSEQ, OUTDSK, DOALIN,
     *   DOKEEP
      HOLLERITH XINNAM(3), XINCLS(2), XOUTNM(3), XOUTCL(2)
      COMMON /INPARM/ XINNAM, XINCLS, INSEQ, INDSK, IN2SEQ, IN3SEQ,
     *   XOUTNM, XOUTCL, OUTSEQ, OUTDSK, DOALIN, DOKEEP
C
      INTEGER   MAXLIS
      PARAMETER (MAXLIS=1000)
C
      CHARACTER INNAM*12, INCLS*6, OUTNAM*12, OUTCLS*6
      INTEGER   ISVOL, ISEQ1, ISEQ2, ISEQ3, IUSER, ISLOT1, INPLAN,
     *   NRDIM, IZPIX(2), IDLUN, IDIND, IDVOL, IDSLOT, IDSEQ, NODIM,
     *   ISVOL1, IHDR1(256), IHDR2(256), ISLOT2, NFQOUT, FQFSN(MAXIMG),
     *   NOD, P1FLOW, ISVOL2, NSKIP, LISKIP(2,MAXLIS), ISLUN, ISIND
      REAL      BLC(7), TRC(7), RHDR1(256), RHDR2(256)
      HOLLERITH HHDR1(256), HHDR2(256)
      REAL      FQBS(MAXIMG)
      DOUBLE PRECISION    DHDR1(128), DHDR2(128), FQFS(MAXIMG)
      COMMON /CHPARM/ INNAM, INCLS, OUTNAM, OUTCLS
      COMMON /MCUCOM/ FQFS, FQBS, FQFSN, ISVOL, ISEQ1, ISEQ2, ISEQ3,
     *   IUSER, ISLOT1, INPLAN,NRDIM, IZPIX, IDLUN, IDIND, IDVOL,
     *   IDSLOT, IDSEQ, NODIM, ISVOL1, BLC, TRC, ISLOT2, NFQOUT, NOD,
     *   P1FLOW, ISVOL2, NSKIP, LISKIP, ISLUN, ISIND
      COMMON /MAPHDR/ IHDR1, IHDR2
      EQUIVALENCE (IHDR1, RHDR1, HHDR1, DHDR1)
      EQUIVALENCE (IHDR2, RHDR2, HHDR2, DHDR2)
      INTEGER   SCRTCH(256), FQBUFF(512)
      REAL      BUFF1(MABFSS), BUFF2(MABFSS)
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH, FQBUFF
LOCAL END
      PROGRAM FQUBE
C-----------------------------------------------------------------------
C! Task to create an n+1 dimensional cube from n dimensional maps
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2013, 2015
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
C   FQUBE (make FQID cube) will create an image cube with FQID as an
C   axis from the input FQID or FREQ axes.  The images must align in a
C   suitable manner with teh FQID or FREQ axis being the highest
C   numbered axis with > 1 point on it.
C   INPUTS: (from Aips)
C      INNAME   R(3)    The catalog entry name for all input files.
C      INCLASS  R(2)    The catalog class of all input files.
C      INSEQ    R       The first catalog sequence number in the
C                       range of input images.
C      INDISK   R       The disk volume number of the source files.
C      IN2SEQ   R       The last sequence number in the set of
C                       input maps. 0 means INSEQ + NPOINTS - 1.
C      IN3SEQ   R       The increment in sequence number to use
C                       in looping from INSEQ to IN2SEQ. 0 => 1
C      OUTNAME  R(3)    The name of the output cube.
C      OUTCLASS R(2)    The class of the output cube.
C      OUTSEQ   R       The sequence number for the output cube.
C      OUTDISK  R       The disk volume number for the output cube.
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FQUBE.INC'
C-----------------------------------------------------------------------
C                                       Get parms, inits, etc.
      CALL QUBINI (IRET)
C                                       write it out
      IF (IRET.EQ.0) CALL QUBWRI (IRET)
C                                       Do history
      IF (IRET.EQ.0) CALL QUBHIS
C
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE QUBINI (IRET)
C-----------------------------------------------------------------------
C   QUBINI performs initializing for FQUBE including creating the
C   output file if needed.  The commons /INPARM/ and /MCUCOM/ are set
C   up here.  The output cube header is placed in IHDR1 in common
C   /MAPHDR/.
C   Outputs:
C      SCRTCH   I(256)   Scratch
C      IRET     I         Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER PRGNAM*6, MTYPE*2, AXTYPE(2)*8
      INTEGER   INPRMS, IERR, IROUND, ISEQ, PFLOW, I, J,
     *   NAX, P2FLOW
      LOGICAL   F
      DOUBLE PRECISION FFLOW, FLOW
      INCLUDE 'FQUBE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'FQUBE '/
      DATA F /.FALSE./
      DATA AXTYPE /'FQID','FREQ'/
C-----------------------------------------------------------------------
C                                       AIPS start up
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
      NSKIP = 0
      ISLUN = 17
C
      INPRMS = 18
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XINNAM, SCRTCH, IRET)
      IF (IRET.GT.0) RQUICK = .TRUE.
      IF (IRET.EQ.1) GO TO 999
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 8
      IDLUN = 16
      CALL RFILL (7, 0.0, BLC)
      CALL RFILL (7, 0.0, TRC)
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      CALL H2CHR (12, 1, XOUTNM, OUTNAM)
      CALL H2CHR (6, 1, XOUTCL, OUTCLS)
C                                       Get map 1
      ISVOL = IROUND (INDSK)
      ISVOL1 = ISVOL
      ISVOL2 = ISVOL
      ISEQ1 = IROUND (INSEQ)
      ISEQ2 = IROUND (IN2SEQ)
      ISEQ3 = IROUND (IN3SEQ)
      IF (ISEQ3.EQ.0) ISEQ3 = 1
      IUSER = NLUSER
      MTYPE = 'MA'
      CALL MAPOPN ('READ', ISVOL1, INNAM, INCLS, ISEQ1, MTYPE, IUSER,
     *   ISLUN, ISIND, ISLOT1, IHDR1, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      ISVOL2 = ISVOL1
      ISLOT2 = ISLOT1
      NFQOUT = 0
      CALL SETAX (IHDR1, HHDR1, RHDR1, DHDR1, NOD, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FQGET (ISEQ1, ISVOL1, ISLOT1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL COPY (256, IHDR1, IHDR2)
      CALL MAPCLS ('READ', ISVOL1, ISLOT1, ISLUN, ISIND, IHDR1, F,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       read all headers and check
      DO 20 ISEQ = ISEQ1+ISEQ3,ISEQ2,ISEQ3
         ISVOL2 = ISVOL
         CALL MAPOPN ('READ', ISVOL2, INNAM, INCLS, ISEQ, MTYPE, IUSER,
     *      ISLUN, ISIND, ISLOT2, IHDR1, SCRTCH, IERR)
         IF (IERR.EQ.0) THEN
            CALL SETAX (IHDR1, HHDR1, RHDR1, DHDR1, I, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL FQGET (ISEQ, ISVOL2, ISLOT2, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL FQMTCH (ISEQ, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL MAPCLS ('READ', ISVOL2, ISLOT2, ISLUN, ISIND, IHDR1, F,
     *         SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 20      CONTINUE
C                                       sort freqs
      FLOW = 1.E20
      PFLOW = 0
      DO 50 I = 1,NFQOUT
         IF (FQFS(I).LT.FLOW) THEN
            FLOW = FQFS(I)
            PFLOW = I
            END IF
 50      CONTINUE
      FFLOW = FLOW
      CALL FILL (NFQOUT, 0, FQFSN)
      FQFSN(PFLOW) = 1
      P1FLOW = PFLOW
      DO 70 J = 2,NFQOUT
         PFLOW = 0
         FLOW = 1.E20
         DO 60 I = 1,NFQOUT
            IF ((FQFSN(I).EQ.0) .AND. (FQFS(I).LT.FLOW)) THEN
               PFLOW = I
               FLOW = FQFS(I)
               END IF
 60         CONTINUE
         FQFSN(PFLOW) = J
         IF (J.EQ.2) P2FLOW = PFLOW
 70      CONTINUE
C                                       create image
      IDVOL = IROUND (OUTDSK)
      IDSEQ = IROUND (OUTSEQ)
      CALL MAKOUT (INNAM, INCLS, ISEQ1, '      ', OUTNAM, OUTCLS, IDSEQ)
      CALL COPY (256, IHDR2, IHDR1)
C                                       fqid axis
      CALL CHR2H (8, AXTYPE(1), 1, HHDR1(KHCTP+2*(NOD-1)))
      IHDR1(KINAX+NOD-1) = NFQOUT
      RHDR1(KRCRP+NOD-1) = 1.0
      RHDR1(KRCIC+NOD-1) = 1.0
      DHDR1(KDCRV+NOD-1) = 1.0D0
C                                       freq axis
      CALL AXEFND (4, AXTYPE(2), IHDR1(KIDIM), HHDR1(KHCTP), NAX, IERR)
      IF (IERR.NE.0) THEN
         NAX = IHDR1(KIDIM)
         IHDR1(KIDIM) = NAX + 1
         END IF
      IHDR1(KINAX+NAX) = 1
      RHDR1(KRCRP+NAX) = 1.0
      RHDR1(KRCIC+NAX) = FQFS(P2FLOW) - FQFS(P1FLOW)
      DHDR1(KDCRV+NAX) = FQFS(P1FLOW)
      CALL CHR2H (8, AXTYPE(2), 1, HHDR1(KHCTP+2*NAX))
      RHDR1(KRDMX) = -1.E20
      RHDR1(KRDMN) = 1.E20
      RHDR1(KRBLK) = 0.0
      RHDR1(KRARP) = 0.0
      DHDR1(KDARV) = 0.0D0
      CALL CHR2H (12, OUTNAM, KHIMNO, HHDR1(KHIMN))
      CALL CHR2H (6, OUTCLS, KHIMCO, HHDR1(KHIMC))
      IHDR1(KIIMS) = IDSEQ
C                                       Create and open
      CALL MCREAT (IDVOL, IDSLOT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = IDVOL
      FCNO(NCFILE) = IDSLOT
      FRW(NCFILE) = 2
      IDSEQ = IHDR1(KIIMS)
C                                       write FQ table
      CALL FQWRIT (IRET)
C
 999  RETURN
      END
      SUBROUTINE SETAX (IHDR, HHDR, RHDR, DHDR, NRD, IRET)
C-----------------------------------------------------------------------
C   SETAX moves all 1 point axes after any multi-point axes.
C   In/out:
C      IHDR   I(256)   Header - integer version
C      RHDR   R(256)   Header - R   version
C      HHDR   H(256)   Header - H   version
C      DHDR   D(128)   Header - D   version
C   Output:
C      NRD    I        Number of the FREQ/FQID axis
C      IRET   I        Error: ok => ok
C                        10 => 1pt axis < 3 and multi-point axis > 2.
C-----------------------------------------------------------------------
      INTEGER   IHDR(256), NRD, IRET
      REAL      RHDR(256)
      HOLLERITH HHDR(256)
      DOUBLE PRECISION DHDR(128)
C
      INTEGER   IND, IH, IL, I, J, FOF
      CHARACTER AXISTP(2)*8
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA AXISTP /'FQID','FREQ'/
C-----------------------------------------------------------------------
      IRET = 0
      IND = IHDR(KIDIM)
 10   IH = 0
         IL = 8
         DO 15 I = 1,IND
            J = KINAX + I - 1
            IF (IHDR(J).LE.1) THEN
               IL = I
            ELSE
               IH = I
               IF (IL.NE.8) GO TO 20
               END IF
 15         CONTINUE
C                                       swap if needed and loop
 20      NRD = IH
         IF (IL.LT.IH) THEN
            IF (IL.GT.2) THEN
               CALL SWAPAX (IHDR, HHDR, RHDR, DHDR, IH, IL, IRET)
               IF (IRET.NE.0) GO TO 999
               GO TO 10
C                                       illegal cube: i/o problem
            ELSE
               WRITE (MSGTXT,1020) IL, IH
               IRET = 10
               GO TO 980
               END IF
            END IF
C                                       find freq axis
      CALL AXEFND (8, AXISTP(1), IHDR(KIDIM), HHDR(KHCTP), FOF, IRET)
      IF (IRET.NE.0) CALL AXEFND (8, AXISTP(2), IHDR(KIDIM),
     *   HHDR(KHCTP), FOF, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'NO FQID OR FREQ AXIS FOUND'
         GO TO 980
      ELSE
         FOF = FOF + 1
         IF (FOF.LT.NRD) THEN
            MSGTXT = 'FREQ/FQID AXIS NOT HIGHEST REAL AXIS'
            IRET = 1
            GO TO 980
         ELSE IF (FOF.GT.NRD) THEN
            IL = NRD + 1
            IH = FOF
            CALL SWAPAX (IHDR, HHDR, RHDR, DHDR, IH, IL, IRET)
            IF (IRET.NE.0) GO TO 999
            NRD = IL
            END IF
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('1-POINT AXIS',I2,' BELOW N-POINT AXIS',I2,' USE TRANS')
      END
      SUBROUTINE FQGET (ISEQ, VOL, SLOT, IRET)
C-----------------------------------------------------------------------
C   FQGET finds the FQID or FREQ axis and lists the actual frequencies
C   in it.
C   Inputs:
C      ISEQ   I   Image seq # (for error messages)
C      VOL    I   Image disk volume
C      SLOT   I   Image disk slot #
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   ISEQ, VOL, SLOT, IRET
C
      INCLUDE 'FQUBE.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   FQOFF, FOFF, NF, VER, LUN, IFQRNO, FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC), NUMIF, FQID, IFSIDE, IREC, NREC, I, IROUND
      LOGICAL   OKAY
      REAL      IFCHW, IFTBW, TEMP
      CHARACTER AXISTP(2)*8
      DOUBLE PRECISION IFFREQ, F0, AFREQ, BFREQ, CFREQ, TFREQ
      CHARACTER BNDCOD*8
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA AXISTP /'FQID','FREQ'/
C-----------------------------------------------------------------------
C                                       find FQID axis
      CALL AXEFND (8, AXISTP(1), IHDR1(KIDIM), HHDR1(KHCTP), FQOFF,
     *   IRET)
      IF (IRET.EQ.0) THEN
         CALL AXEFND (8, AXISTP(2), IHDR1(KIDIM), HHDR1(KHCTP), FOFF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) ISEQ
            GO TO 980
            END IF
         IF (IHDR1(KINAX+FOFF).NE.1) THEN
            WRITE (MSGTXT,1010) ISEQ, IHDR1(KINAX+FOFF)
            IRET = 2
            GO TO 980
            END IF
         DO 10 I = FQOFF+1,IHDR1(KIDIM)-1
            IF (IHDR1(KINAX+I).GT.1) THEN
               WRITE (MSGTXT,1015) ISEQ, I+1
               IRET = 2
               GO TO 980
               END IF
 10         CONTINUE
         NF = IHDR1(KINAX+FQOFF)
         F0 = DHDR1(KDCRV+FOFF) + (1.0 - RHDR1(KRCRP+FOFF)) *
     *      RHDR1(KRCIC+FOFF)
C                                       open FQ table
         VER = 1
         LUN = 20
         NUMIF = 1
         CALL FQINI ('READ', FQBUFF, VOL, SLOT, VER, IHDR1, LUN, IFQRNO,
     *      FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
         NREC = FQBUFF(5)
         IF (NREC.NE.NF) THEN
            WRITE (MSGTXT,1020) ISEQ, NREC, NF
            IRET = 3
            GO TO 970
            END IF
C                                       read all
         OKAY = .TRUE.
         DO 30 IREC = 1,NREC
            IFQRNO = IREC
            CALL TABFQ ('READ', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) GO TO 999
            TEMP = (FQID - DHDR1(KDCRV+FQOFF)) / RHDR1(KRCIC+FQOFF) +
     *         RHDR1(KRCRP+FQOFF)
            I = IROUND (TEMP)
            IF ((I.LT.1) .OR. (I.GT.NF)) THEN
               WRITE (MSGTXT,1030) ISEQ, IREC, I
               IRET = 4
               GO TO 970
               END IF
            IF (DOKEEP.LE.0.0) THEN
               TFREQ = F0 + IFFREQ
               CALL CHKPLN (TFREQ, OKAY, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            IF (OKAY) THEN
               NFQOUT = NFQOUT + 1
               FQFS(NFQOUT) = F0 + IFFREQ
               FQBS(NFQOUT) = IFCHW
            ELSE
               NSKIP = NSKIP + 1
               LISKIP(1,NSKIP) = ISEQ
               LISKIP(2,NSKIP) = I
               END IF
 30         CONTINUE
         IRET = 0
         GO TO 975
C                                       freq axis only
      ELSE
         CALL AXEFND (8, AXISTP(2), IHDR1(KIDIM), HHDR1(KHCTP), FOFF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) ISEQ
            GO TO 980
            END IF
         DO 50 I = FOFF+1,IHDR1(KIDIM)-1
            IF (IHDR1(KINAX+I).GT.1) THEN
               WRITE (MSGTXT,1055) ISEQ, I+1
               IRET = 2
               GO TO 980
               END IF
 50         CONTINUE
         NF = IHDR1(KINAX+FOFF)
         AFREQ = RHDR1(KRCIC+FOFF)
         BFREQ = RHDR1(KRCRP+FOFF)
         OKAY = .TRUE.
         DO 60 I = 1,NF
            CFREQ = I - BFREQ
            TFREQ = DHDR1(KDCRV+FOFF) + CFREQ * AFREQ
            IF (DOKEEP.LE.0.0) THEN
               CALL CHKPLN (TFREQ, OKAY, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            IF (OKAY) THEN
               NFQOUT = NFQOUT + 1
               FQFS(NFQOUT) = TFREQ
               FQBS(NFQOUT) = ABS (RHDR1(KRCIC+FOFF))
            ELSE
               NSKIP = NSKIP + 1
               LISKIP(1,NSKIP) = ISEQ
               LISKIP(2,NSKIP) = I
               END IF
 60         CONTINUE
         GO TO 999
         END IF
C                                       close
C
 970  CALL MSGWRT (8)
 975  CALL TABFQ ('CLOS', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *   FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, VER)
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SEQ #',I5,' HAS FQID AXIS BUT NO FREQ AXIS')
 1010 FORMAT ('SEQ #',I5,' HAS FQID AXIS BUT FREQ AXIS HAS',I5,
     *   ' PIXELS')
 1015 FORMAT ('SEQ #',I5,' FQID AXIS NOT HIGHEST *REAL* AXIS',I2)
 1020 FORMAT ('SEQ #',I5,' HAS FQID AXIS BUT FQID AXIS AND FQ TABLE',
     *   2I4,' DIFFER')
 1030 FORMAT ('SEQ #',I5,' FQ REC',I3,' FQID POINT',I4,' OUTSIDE AXIS')
 1050 FORMAT ('SEQ #',I5,' HAS NEITHER AN FQID OR A FREQ AXIS')
 1055 FORMAT ('SEQ #',I5,' FREQ AXIS NOT HIGHEST *REAL* AXIS',I2)
      END
      SUBROUTINE FQMTCH (ISEQ, IRET)
C-----------------------------------------------------------------------
C   FQMTCH checks the coordinate alignment of the 2 images
C   Inputs:
C      ISEQ     I   Input image seq number
C   Outputs:
C      IRET     I   Error code: > 0 no match
C-----------------------------------------------------------------------
      INTEGER   ISEQ, IRET
C
      INCLUDE 'FQUBE.INC'
      INTEGER   I, J, FOF1, FOF2
      REAL      TEMP
      CHARACTER AXISTP(2)*8, CTEST1*8, CTEST2*8
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA AXISTP /'FQID','FREQ'/
C-----------------------------------------------------------------------
      CALL AXEFND (8, AXISTP(1), IHDR1(KIDIM), HHDR1(KHCTP), FOF1, IRET)
      IF (IRET.NE.0) CALL AXEFND (8, AXISTP(2), IHDR1(KIDIM),
     *   HHDR1(KHCTP), FOF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) ISEQ
         GO TO 980
         END IF
      CALL AXEFND (8, AXISTP(1), IHDR2(KIDIM), HHDR2(KHCTP), FOF2, IRET)
      IF (IRET.NE.0) CALL AXEFND (8, AXISTP(2), IHDR2(KIDIM),
     *   HHDR2(KHCTP), FOF2, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'FIRST IMAGE LACKS AN FQID OR FREQ AXIS'
         GO TO 980
         END IF
      DO 20 I = 1,7
         IRET = 10 + I
         J = I - 1
C                                       > 1-pt axes  > freq ?
         IF ((J.GT.FOF1) .OR. (J.GT.FOF2)) THEN
            IF (((I.LE.IHDR1(KIDIM)) .AND. (IHDR1(KINAX+J).GT.1)) .OR.
     *         ((I.LE.IHDR2(KIDIM)) .AND. (IHDR2(KINAX+J).GT.1))) THEN
               WRITE (MSGTXT,1010) ISEQ
               GO TO 980
               END IF
         ELSE IF ((J.LT.FOF1) .OR. (J.LT.FOF2)) THEN
            IF (IHDR1(KINAX+J).NE.IHDR2(KINAX+J)) THEN
               WRITE (MSGTXT,1015) ISEQ, I, 'NUMBER PIXELS'
               GO TO 980
               END IF
            IF (DOALIN.GT.-1.9) THEN
               CALL H2CHR (8, 1, HHDR1(KHCTP+2*J), CTEST1)
               CALL H2CHR (8, 1, HHDR2(KHCTP+2*J), CTEST2)
               IF (CTEST1.NE.CTEST2) THEN
                  WRITE (MSGTXT,1015) ISEQ, I, 'AXIS TYPE'
                  GO TO 980
                  END IF
               END IF
            IF (DOALIN.GT.0.0) THEN
               TEMP = ABS (DHDR2(KRCIC+J)) * 0.01
               IF (TEMP.EQ.0.0) TEMP = 0.001
               IF (ABS(DHDR1(KDCRV+J)-DHDR2(KDCRV+J)).GT.TEMP) THEN
                  WRITE (MSGTXT,1015) ISEQ, I, 'REFERENCE VALUE'
                  GO TO 980
                  END IF
               IF (ABS(RHDR1(KRCIC+J)-RHDR2(KRCIC+J)).GT.TEMP) THEN
                  WRITE (MSGTXT,1015) ISEQ, I, 'INCREMENT'
                  GO TO 980
                  END IF
               IF (ABS(RHDR1(KRCRP+J)-RHDR2(KRCRP+J)).GT.0.01) THEN
                  WRITE (MSGTXT,1015) ISEQ, I, 'REFERENCE PIXEL'
                  GO TO 980
                  END IF
               END IF
            END IF
 20      CONTINUE
      IRET = 0
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMAGE',I4,' LACKS AN FQID OR FREQ AXIS')
 1010 FORMAT ('IMAGE',I4,' >1 POINT AXES FOLLOW FQID OR FREQ')
 1015 FORMAT ('IMAGE',I4,' AXIS',I2,' UNEQUAL ',A)
      END
      SUBROUTINE FQWRIT (IRET)
C-----------------------------------------------------------------------
C   FQWRIT writes an FQ table
C   Inputs all through common
C   Output
C      IRET     I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, LUN, IFQRNO, FQKOLS(MAXFQC), FQNUMV(MAXFQC), NUMIF,
     *   FQID, IFSIDE, I, KEY(2,2), KEYSUB(2,2)
      DOUBLE PRECISION IFFREQ
      REAL      IFCHW, IFTBW, FKEY(2,2)
      CHARACTER BNDCOD*8
      INCLUDE 'FQUBE.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA KEY /1,0, 2,0/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       open
      VER = 1
      LUN = 20
      NUMIF = 1
      CALL FQINI ('WRIT', FQBUFF, IDVOL, IDSLOT, VER, IHDR1, LUN,
     *   IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'ERROR CREATING FQ TABLE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       write
      IFSIDE = 1
      DO 20 I = 1,NFQOUT
         IFQRNO = I
         IFFREQ = FQFS(I) - FQFS(P1FLOW)
         FQID = FQFSN(I)
         IFCHW = FQBS(I)
         IFTBW = FQBS(I)
         BNDCOD = ' '
         CALL TABFQ ('WRIT', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR WRITING FQ TABLE'
            CALL MSGWRT (8)
            GO TO 999
            END IF
 20      CONTINUE
C                                       close
      CALL TABFQ ('CLOS', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF, FQID,
     *   IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, VER)
C                                       sort
      CALL TABSRT (IDVOL, IDSLOT, 'FQ', VER, VER, KEY, KEYSUB, FKEY,
     *   FQBUFF, IHDR1, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'ERROR SORTING FQ TABLE'
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
      END
      SUBROUTINE QUBWRI (IRET)
C-----------------------------------------------------------------------
C   writes the output cube from the input cubes
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FQUBE.INC'
      INTEGER   II(7), NN(7), IDEPTH(5), IBLKOF, IPLANE, NX, NY, ISEQ,
     *   IWIN(4), NBY, ISPOS, IDPOS, I7, I6, I5, I4, I3, I2, I1,
     *   ICGVER, LSKIP
      CHARACTER MTYPE*2, CTYPE*8
      LOGICAL   WASBLK, DOCGEX
      REAL      RMAX, RMIN, TEMP, OUAREA, INAREA, SCALE
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (I1, II(1)), (I2, II(2)), (I3, II(3)), (I4, II(4))
      EQUIVALENCE (I5, II(5)), (I6, II(6)), (I7, II(7))
C-----------------------------------------------------------------------
C                                       open output
      MTYPE = 'MA'
      CALL MAPOPN ('INIT', IDVOL, OUTNAM, OUTCLS, IDSEQ, MTYPE, IUSER,
     *   IDLUN, IDIND, IDSLOT, IHDR1, SCRTCH, IRET)
C                                       process header
      CALL H2CHR (8, 1, HHDR1(KHBUN), CTYPE)
      CALL CHLTOU (8, CTYPE)
      IF ((CTYPE.EQ.'JY/BEAM') .AND. (RHDR1(KRBMJ).GT.0.0) .AND.
     *   (RHDR1(KRBMN).GT.0.0)) THEN
         OUAREA = RHDR1(KRBMJ) * RHDR1(KRBMN)
      ELSE
         OUAREA = 0.0
         END IF
      NX = IHDR2(KINAX)
      NY = IHDR2(KINAX+1)
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = NX
      IWIN(4) = NY
      NBY = 2 * MABFSS
      IPLANE = 0
      WASBLK = .FALSE.
      RMAX = -1.E20
      RMIN = -RMAX
      LSKIP = 1
      DO 100 ISEQ = ISEQ1,ISEQ2,ISEQ3
         ISVOL2 = ISVOL
         CALL MAPOPN ('READ', ISVOL2, INNAM, INCLS, ISEQ, MTYPE, IUSER,
     *      ISLUN, ISIND, ISLOT2, IHDR2, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 100
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = ISVOL2
         FCNO(NCFILE) = ISLOT2
         FRW(NCFILE) = 0
         CALL H2CHR (8, 1, HHDR2(KHBUN), CTYPE)
         CALL CHLTOU (8, CTYPE)
         IF ((CTYPE.EQ.'JY/BEAM') .AND. (RHDR2(KRBMJ).GT.0.0) .AND.
     *      (RHDR2(KRBMN).GT.0.0)) THEN
            INAREA = RHDR2(KRBMJ) * RHDR2(KRBMN)
         ELSE
            INAREA = 0.0
            END IF
         IF ((INAREA.GT.0.0) .AND. (OUAREA.GT.0.0)) THEN
            SCALE = OUAREA / INAREA
            DOCGEX = .TRUE.
         ELSE
            SCALE = 1.0
            DOCGEX = .FALSE.
            END IF
         CALL FNDEXT ('CG', IHDR2, ICGVER)
         WRITE (MSGTXT,1010) ISEQ, SCALE
         IF (SCALE.NE.1.0) CALL MSGWRT (3)
         CALL COPY (7, IHDR2(KINAX), NN)
         CALL FILL (7-NOD, 1, NN(NOD+1))
         DO 70 I7 = 1,NN(7)
            IF (NOD.EQ.7) THEN
               IF ((LISKIP(1,LSKIP).EQ.ISEQ) .AND.
     *            (I7.EQ.LISKIP(2,LSKIP))) THEN
                  LSKIP = LSKIP + 1
                  GO TO 70
               ELSE
                  IPLANE = IPLANE + 1
                  END IF
               END IF
         DO 60 I6 = 1,NN(6)
            IF (NOD.EQ.6) THEN
               IF ((LISKIP(1,LSKIP).EQ.ISEQ) .AND.
     *            (I6.EQ.LISKIP(2,LSKIP))) THEN
                  LSKIP = LSKIP + 1
                  GO TO 60
               ELSE
                  IPLANE = IPLANE + 1
                  END IF
               END IF
         DO 50 I5 = 1,NN(5)
            IF (NOD.EQ.5) THEN
               IF ((LISKIP(1,LSKIP).EQ.ISEQ) .AND.
     *            (I5.EQ.LISKIP(2,LSKIP))) THEN
                  LSKIP = LSKIP + 1
                  GO TO 50
               ELSE
                  IPLANE = IPLANE + 1
                  END IF
               END IF
         DO 40 I4 = 1,NN(4)
            IF (NOD.EQ.4) THEN
               IF ((LISKIP(1,LSKIP).EQ.ISEQ) .AND.
     *            (I4.EQ.LISKIP(2,LSKIP))) THEN
                  LSKIP = LSKIP + 1
                  GO TO 40
               ELSE
                  IPLANE = IPLANE + 1
                  END IF
               END IF
         DO 30 I3 = 1,NN(3)
            IF (NOD.EQ.3) THEN
               IF ((LISKIP(1,LSKIP).EQ.ISEQ) .AND.
     *            (I3.EQ.LISKIP(2,LSKIP))) THEN
                  LSKIP = LSKIP + 1
                  GO TO 30
               ELSE
                  IPLANE = IPLANE + 1
                  END IF
               END IF
            CALL COPY (5, II(3), IDEPTH)
            CALL COMOFF (IHDR2(KIDIM), IHDR2(KINAX), IDEPTH, IBLKOF,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            IBLKOF = IBLKOF + 1
C                                       Initialize for double buffering
            CALL MINIT ('READ', ISLUN, ISIND, NX, NY, IWIN, BUFF2,
     *         NBY, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, ISEQ, 'INIT READ'
               GO TO 980
               END IF
C                                       output "plane"
            IDEPTH(NOD-2) = FQFSN(IPLANE)
            CALL COMOFF (IHDR1(KIDIM), IHDR1(KINAX), IDEPTH, IBLKOF,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            IBLKOF = IBLKOF + 1
C                                       Initialize for double buffering
            CALL MINIT ('WRIT', IDLUN, IDIND, NX, NY, IWIN, BUFF1,
     *         NBY, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, ISEQ, 'INIT WRITE'
               GO TO 980
               END IF
            DO 20 I2 = 1,NN(2)
C                                       The first write sets up indicies
               CALL MDISK ('WRIT', IDLUN, IDIND, BUFF1, IDPOS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, ISEQ, 'WRITE REC'
                  GO TO 980
                  END IF
C                                       read all rows
               CALL MDISK ('READ', ISLUN, ISIND, BUFF2, ISPOS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, ISEQ, 'READ REC'
                  GO TO 980
                  END IF
C                                       copy w max/min
               DO 10 I1 = 1,NN(1)
                  TEMP  = BUFF2(ISPOS+I1-1)
                  IF (TEMP.EQ.FBLANK) THEN
                     WASBLK = .TRUE.
                  ELSE
                     TEMP = TEMP * SCALE
                     RMIN = MIN (RMIN, TEMP)
                     RMAX = MAX (RMAX, TEMP)
                     END IF
                  BUFF1(IDPOS+I1-1) = TEMP
 10               CONTINUE
 20            CONTINUE
C                                       CG file
            IF (DOCGEX) CALL FQUBCG (FQFS(IPLANE), RHDR2(KRBMJ),
     *         RHDR2(KRBMN), RHDR2(KRBPA), ICGVER, IRET)
C                                       finish
            CALL MDISK ('FINI', IDLUN, IDIND, BUFF1, IDPOS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, ISEQ, 'FINI PLANE'
               GO TO 980
               END IF
 30         CONTINUE
 40         CONTINUE
 50         CONTINUE
 60         CONTINUE
 70         CONTINUE

         CALL MAPCLS ('READ', ISVOL2, ISLOT2, ISLUN, ISIND, IHDR2,
     *      .FALSE., SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 999
         NCFILE = NCFILE - 1
 100     CONTINUE
C                                       update header
      RHDR1(KRDMX) = RMAX
      RHDR1(KRDMN) = RMIN
      IF (WASBLK) RHDR1(KRBLK) = FBLANK
C                                       Update CATBLK.
      CALL MAPCLS ('INIT', IDVOL, IDSLOT, IDLUN, IDIND, IHDR1, .TRUE.,
     *   SCRTCH, IRET)
      NCFILE = NCFILE - 1
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I4,' SEQ',I4,' DOING ',A)
 1010 FORMAT ('QUBWRI: scaling sequence number',I5,' by',F8.4)
      END
      SUBROUTINE QUBHIS
C-----------------------------------------------------------------------
C   QUBHIS copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, NOTYPE(2)*2
      INTEGER   LUN1, LUN2, IERR, NONOT
      INCLUDE 'FQUBE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NONOT, NOTYPE /2, 'FQ','CG'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, ISVOL, IDVOL, ISLOT1, IDSLOT, IHDR1,
     *   SCRTCH, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, INNAM, INCLS, ISEQ1, ISVOL, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, OUTNAM, OUTCLS, IDSEQ, IDVOL, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1010) TSKNAM, ISEQ2
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.GT.0) GO TO 100
      WRITE (HILINE,1011) TSKNAM, ISEQ3
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.GT.0) GO TO 100
      WRITE (HILINE,1012) TSKNAM, DOALIN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.GT.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTYPE, LUN1, LUN2, ISVOL, IDVOL, ISLOT1,
     *   IDSLOT, IHDR1, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'QUBHIS: ERROR COPYING TABLES TO OUTPUT'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUBHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'INSEQ2   =',I6,5X,'/ seq = inseq1:inseq2 by inseq3')
 1011 FORMAT (A6,'INSEQ3   =',I6,5X,'/ seq = inseq1:inseq2 by inseq3')
 1012 FORMAT (A6,'DOALIGN  =',F6.1,5X,'/ alignment required?')
      END
      SUBROUTINE FQUBCG (FREQ, BMAJ, BMIN, BPA, ICGVER, IERR)
C-----------------------------------------------------------------------
C   Writes CG table
C   Inputs:
C      FREQ     D   Frequency of entry
C      BMAJ     R   Major axis of input header
C      BMIN     R   Minor axis of input header
C      BPA      R   Postion angle of input header
C      ICGVER   I   Version number of GC table in input
C   Outputs:
C      IERR     I   Error code
C-----------------------------------------------------------------------
      DOUBLE PRECISION FREQ
      REAL      BMAJ, BMIN, BPA
      INTEGER   ICGVER, IERR
C
      INTEGER   CGBUFF(512), VER, LUNCG, ICGRNO, CGKOLS(4), CGNUMV(4),
     *   TABVER, NROW, I
      REAL      BMA, BMN, BP
      DOUBLE PRECISION FF
      INCLUDE 'FQUBE.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNCG /51/
C-----------------------------------------------------------------------
      VER = 0
C                                       find in input CG table
      IF (ICGVER.GT.0) THEN
         CALL CGINI ('READ', CGBUFF, ISVOL2, ISLOT2, VER, IHDR2, LUNCG,
     *      ICGRNO, CGKOLS, CGNUMV, TABVER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN OLD CG TABLE'
            GO TO 990
            END IF
         NROW = CGBUFF(5)
         DO 10 I = 1,NROW
            CALL TABCG ('READ', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA,
     *         BMN, BP, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ OLD CG TABLE'
               GO TO 990
            ELSE IF (IERR.EQ.0) THEN
               IF (ABS(FREQ-FF).LT.10.0D0) GO TO 20
               END IF
 10         CONTINUE
         ICGRNO = 1
         DO 15 I = 1,NROW
            CALL TABCG ('READ', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA,
     *         BMN, BP, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ OLD CG TABLE'
               GO TO 990
            ELSE IF (IERR.EQ.0) THEN
               IF (ABS(FREQ-FF).LT.500.0D0) GO TO 20
               END IF
 15         CONTINUE
         WRITE (MSGTXT,1010) FREQ
         CALL MSGWRT (6)
         FF = 0.0D0
         IERR = 0
 20      CALL TABCG ('CLOS', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA,
     *      BMN, BP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CLOSE OLD CG TABLE'
            GO TO 990
            END IF
         IF (FF.LT.10.0D0) GO TO 999
      ELSE
         FF = FREQ
         BMA = BMAJ
         BMN = BMIN
         BP  = BPA
         END IF
C                                       write line to new CG table
      CALL CGINI ('WRIT', CGBUFF, IDVOL, IDSLOT, VER, IHDR1, LUNCG,
     *   ICGRNO, CGKOLS, CGNUMV, TABVER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN NEW CG TABLE'
         GO TO 990
         END IF
      CALL TABCG ('WRIT', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA, BMN,
     *   BP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITE NEW CG TABLE'
         GO TO 990
         END IF
      CALL TABCG ('CLOS', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA, BMN,
     *   BP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSE NEW CG TABLE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FQUBCG: ERROR',I3,' ON ',A)
 1010 FORMAT ('FQUBCG: COULD NOT FIND FREQ',1PE13.6,' IN OLD CG TABLE')
      END
      SUBROUTINE CHKPLN (TFREQ, OKAY, IRET)
C-----------------------------------------------------------------------
C   CHKPLN checks whether the "plane" has some value besides 0.0 or
C   blank and whether it has an entry in the CG table.  If not, it
C   returns OKAY as false.
C   Inputs:
C      TFREQ   D   Frequency to find in the CG table
C   Outputs:
C      OKAY    L   True => use this plane
C      IRET    I   Error code from disk IO issues
C-----------------------------------------------------------------------
      INTEGER    IRET
      LOGICAL    OKAY
      DOUBLE PRECISION TFREQ
C
      INTEGER    II(7), NN(7), IDEPTH(5), IBLKOF, NX, NY, ISEQ, IWIN(4),
     *   NBY, ISPOS, I7, I6, I5, I4, I3, I2, I1, ICGVER, CGBUFF(512),
     *   VER, LUNCG, ICGRNO, CGKOLS(4), CGNUMV(4), TABVER, NROW, I
      REAL      BMA, BMN, BP, RMAX, RMIN, TEMP
      DOUBLE PRECISION FF
      INCLUDE 'FQUBE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (I1, II(1)), (I2, II(2)), (I3, II(3)), (I4, II(4))
      EQUIVALENCE (I5, II(5)), (I6, II(6)), (I7, II(7))
      DATA LUNCG /51/
C-----------------------------------------------------------------------
C                                       start by looking for CG table
      VER = 0
C                                       find in input CG table
      OKAY = .TRUE.
      CALL FNDEXT ('CG', IHDR1, ICGVER)
      IF (ICGVER.GT.0) THEN
         CALL CGINI ('READ', CGBUFF, ISVOL2, ISLOT2, VER, IHDR1, LUNCG,
     *      ICGRNO, CGKOLS, CGNUMV, TABVER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OLD CG TABLE'
            GO TO 990
            END IF
         NROW = CGBUFF(5)
         DO 10 I = 1,NROW
            CALL TABCG ('READ', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA,
     *         BMN, BP, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ OLD CG TABLE'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               IF (ABS(TFREQ-FF).LT.99.0D0) GO TO 20
               END IF
 10         CONTINUE
         OKAY = .FALSE.
 20      CALL TABCG ('CLOS', CGBUFF, ICGRNO, CGKOLS, CGNUMV,
     *      FF, BMA,BMN, BP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE OLD CG TABLE'
            GO TO 990
            END IF
         END IF
C                                       read plane
      IF (OKAY) THEN
         NX = IHDR1(KINAX)
         NY = IHDR1(KINAX+1)
         IWIN(1) = 1
         IWIN(2) = 1
         IWIN(3) = NX
         IWIN(4) = NY
         NBY = 2 * MABFSS
         RMAX = -1.E20
         RMIN = -RMAX
         CALL COPY (7, IHDR1(KINAX), NN)
         CALL FILL (7-NOD, 1, NN(NOD+1))
         DO 90 I7 = 1,NN(7)
         DO 80 I6 = 1,NN(6)
         DO 70 I5 = 1,NN(5)
         DO 60 I4 = 1,NN(4)
         DO 50 I3 = 1,NN(3)
            CALL COPY (5, II(3), IDEPTH)
            CALL COMOFF (IHDR1(KIDIM), IHDR1(KINAX), IDEPTH, IBLKOF,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            IBLKOF = IBLKOF + 1
C                                       Initialize for double buffering
            CALL MINIT ('READ', ISLUN, ISIND, NX, NY, IWIN, BUFF2,
     *         NBY, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, ISEQ, 'INIT READ'
               GO TO 990
               END IF
            DO 40 I2 = 1,NN(2)
C                                       read all rows
               CALL MDISK ('READ', ISLUN, ISIND, BUFF2, ISPOS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, ISEQ, 'READ REC'
                  GO TO 990
                  END IF
C                                       copy w max/min
               DO 30 I1 = 1,NN(1)
                  TEMP  = BUFF2(ISPOS+I1-1)
                  IF (TEMP.NE.FBLANK) THEN
                     RMAX = MAX (RMAX, TEMP)
                     RMIN = MIN (RMIN, TEMP)
                     IF (RMAX.GT.RMIN) GO TO 999
                     END IF
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
 60         CONTINUE
 70         CONTINUE
 80         CONTINUE
 90         CONTINUE
         OKAY = .FALSE.
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHKPLN: ERROR',I4,' ON ',A)
      END
