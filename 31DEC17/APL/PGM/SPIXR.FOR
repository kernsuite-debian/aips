LOCAL INCLUDE 'XMBUFRS'
      INCLUDE 'INCS:PMAD.INC'
      REAL     BUFF1(MABFSS), BUFF2(MABFSS), SCRTCH(512)
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
LOCAL END
LOCAL INCLUDE 'SPIXR.INC'
      INCLUDE 'XMBUFRS'
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, OPTYPE*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XOPTYP, CATOH(256)
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   FCUT, REFREQ, PBPARM(7), DPARM(10), BADD(10)
      DOUBLE PRECISION CATOD(128), FV, RV, DV, RA0, DE0, XFREQ(MAXIMG),
     *   DEFREQ
      REAL      CATOR(256), PMIN(7), PMAX(7), FI, RI, DI, FR, RR, DR,
     *   MROT
      INTEGER   CATOLD(256), SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, FAX, RAX, DAX, NGOOD
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XSEQO,
     *   XDISKO, BLC, TRC, FCUT, REFREQ, XOPTYP, PBPARM, DPARM, BADD
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, OPTYPE
      COMMON /PARMS/ CATOLD, FV, RV, DV, RA0, DE0, XFREQ, DEFREQ, PMAX,
     *   PMIN, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO, JBUFSZ, FI,
     *   RI, DI, FR, RR, DR, FAX, RAX, DAX, MROT, NGOOD
      EQUIVALENCE (CATOLD, CATOR, CATOH, CATOD)
LOCAL END
      PROGRAM SPIXR
C-----------------------------------------------------------------------
C! SPIXR fits 1-D spectral indexes
C# Map-util Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2005-2006, 2008, 2010, 2012, 2015, 2017
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
C   SPIXR fits 1-dimensional spectral inices to rows of an image.  It
C   fits
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      FLUX           FCUT          Flux cutoff: use only data >
C                                   FLUX.
C      OPTYPE         XOPTYP        '': Blank illegal velocities;
C      BADD(10)       IBAD          Disk numbers to avoid.
C   Programmer Eric W. Greisen  2005
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      LOGICAL   ABLANK
      INCLUDE 'SPIXR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'SPIXR'/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SPIXIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL SPIXDO (ABLANK, IRET)
      IF (IRET.EQ.0) CALL SPIXOU (ABLANK, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE SPIXIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   SPIXIN gets input parameters for SPIXR.
C   Inputs:
C      PRGN   C*6   Program name (2 chars/word)
C   Output:
C      IRET   I      Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C                               <0 => failed to get all frequencies
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER   STAT*4, PRGN*6, OTYPE*8, CLAOUT*6, CUNITS(5)*8,
     *   SEQTYP(7)*8, CTEMP*8, MTYPE*2, C1TYP*8
      INTEGER  IRET, IPT, I, IERR, NPARM, IROUND, IG, INC, ITYP, NAX
      DOUBLE PRECISION C1CRV
      REAL      CONST, C1CRP, C1CIC, C1CRT
      INCLUDE 'SPIXR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA CUNITS /'SP COUNT', 2 * 'SP INDEX', 2 * 'CSPINDEX'/
      DATA SEQTYP /'B TEMP', 'D BT', 'SP CNT', 'SPIX','D SPIX',
     *   'SPCU', 'D SPCU'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
C                                       Fixed PPM 1996.09.30: was 38
      NPARM = 56
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (12, 1, XOPTYP, OPTYPE)
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
      DO 20 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 20      CONTINUE
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get user modification to CATBLK
      IRET = 4
      CALL SPIHED (IRET)
      IF (IRET.NE.0) GO TO 999
      NEWCNO = 0
C                                       Make names, classes, disks OK.
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT)
      CONST = -1.E15
      CALL RFILL (7, CONST, PMAX)
      CONST = -CONST
      CALL RFILL (7, CONST, PMIN)
C                                       PBCORR parameters
      FAX = 0
      RAX = 0
      DAX = 0
      NAX = CATOLD(KIDIM)
      DO 100 I = 1,NAX
         CALL H2CHR (8, 1, CATOH(KHCTP+2*(I-1)), OTYPE)
         IF (OTYPE(:4).EQ.'FREQ') THEN
            FAX = I
            FV = CATOD(KDCRV+I-1)
            FI = CATOR(KRCIC+I-1)
            FR = CATOR(KRCRP+I-1)
         ELSE IF (OTYPE(:8).EQ.'SEQ.NUM.') THEN
            FAX = I
         ELSE IF (OTYPE(:8).EQ.'FQID') THEN
            FAX = I
         ELSE IF ((OTYPE(:2).EQ.'RA') .OR. (OTYPE(2:4).EQ.'LON')) THEN
            RAX = I
            RV = CATOD(KDCRV+I-1) * DG2RAD
            RI = CATOR(KRCIC+I-1) * DG2RAD
            RR = CATOR(KRCRP+I-1)
         ELSE IF ((OTYPE(:3).EQ.'DEC') .OR. (OTYPE(2:4).EQ.'LAT')) THEN
            DAX = I
            DV = CATOD(KDCRV+I-1) * DG2RAD
            DI = CATOR(KRCIC+I-1) * DG2RAD
            DR = CATOR(KRCRP+I-1)
            MROT = CATOR(KRCRT+I-1) * DG2RAD
            END IF
 100     CONTINUE
      IF ((PBPARM(1).GT.0.0) .AND. (DAX*RAX*FAX.LE.0)) THEN
         MSGTXT = 'FREQ/RA/DEC AXIS NOT FOUND: PBPARM TURNED OFF'
         CALL MSGWRT (7)
         PBPARM(1) = 0.0
         END IF
      RA0 = CATOD(KDORA) * DG2RAD
      DE0 = CATOD(KDODE) * DG2RAD
      IF ((RA0.EQ.0.0D0) .AND. (DE0.EQ.0.0D0)) THEN
         RA0 = RV
         DE0 = DV
         END IF
C                                       reference freq
      IF (FAX.LE.0) THEN
         IF (REFREQ.LE.0.0) REFREQ = 1.0
      ELSE IF (REFREQ.LT.0.0) THEN
         REFREQ = FV / 1.D9
      ELSE IF (REFREQ.EQ.0.0) THEN
         REFREQ = 1.0
         END IF
      DEFREQ = REFREQ * 1.D9
C                                       create output files in advance
C                                       Basic output header
      NAX = CATBLK(KIDIM) - 1
      INC = 2
C                                       save averaged axis for what??
      C1CRP = (CATR(KRCRP) - 1.5) / CATBLK(KINAX) + 0.5
      C1CIC = CATR(KRCIC) * CATBLK(KINAX)
      C1CRT = CATR(KRCRT)
      C1CRV = CATD(KDCRV)
      CALL H2CHR (8, 1, CATH(KHCTP), C1TYP)
C                                       move other axes down
      DO 60 I = 1,NAX
         CATBLK(KINAX+I-1) = CATBLK(KINAX+I)
         CATR(KRCRP+I-1) = CATR(KRCRP+I)
         CATR(KRCRT+I-1) = CATR(KRCRT+I)
         CATR(KRCIC+I-1) = CATR(KRCIC+I)
         CATD(KDCRV+I-1) = CATD(KDCRV+I)
         IPT = KHCTP+I*INC
         CALL H2CHR (8, 1, CATH(IPT), CTEMP)
         IPT = KHCTP+(I-1)*INC
         CALL CHR2H (8, CTEMP, 1, CATH(IPT))
 60      CONTINUE
      CATR(KRCRP+NAX) = 1.0
      CATR(KRCRT+NAX) = 0.0
      CATR(KRCIC+NAX) = 1.0
      CATD(KDCRV+NAX) = DEFREQ
      IPT = KHCTP + NAX*INC
      CALL CHR2H (8, C1TYP, 1, CATH(IPT))
      DO 65 I = NAX,6
         CATBLK(KINAX+I) = 1
 65      CONTINUE
C                                       creates
      ITYP = 5
      IF (OPTYPE.EQ.'CURV') ITYP = 7
      DO 90 IG = 1,ITYP
         CALL CHR2H (6, SEQTYP(IG), KHIMCO, CATH(KHIMC))
         IF (IG.LE.2) THEN
            CALL H2CHR (8, 1, CATOH(KHBUN), CTEMP)
            CALL CHR2H (8, CTEMP, 1, CATH(KHBUN))
            END IF
         IF (IG.GT.2) CALL CHR2H (8, CUNITS(IG-2), 1, CATH(KHBUN))
C                                       Create
         DISKO = XDISKO + 0.01
         NEWCNO = 1
         CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1075) IERR, SEQTYP(IG)
            GO TO 990
            END IF
C                                       Record the creation
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = NEWCNO
         FRW(NCFILE) = 2
         SEQOUT = CATBLK(KIIMS)
C                                       copy most keywords
         CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
 90      CONTINUE
      IRET = 0
C                                       get frequencies
      NAX = CATOLD(KINAX)
      CALL H2CHR (8, 1, CATOH(KHCTP), OTYPE)
      IF (OTYPE(:4).EQ.'FREQ') THEN
         DO 110 I = 1,NAX
            XFREQ(I) = FV + FI * (I - FR)
            XFREQ(I) = LOG10 (XFREQ(I)/DEFREQ)
 110        CONTINUE
      ELSE IF (OTYPE.EQ.'SEQ.NUM.') THEN
         CALL HIGET (DISKIN, OLDCNO, NAX, DEFREQ, XFREQ, IRET)
      ELSE
         C1CRV = CATOD(KDCRV)
         C1CRP = CATOR(KRCRP)
         C1CIC = CATOR(KRCIC)
         CALL FQGET (DISKIN, OLDCNO, NAX, FV, C1CRV, C1CRP, C1CIC,
     *      CATOLD, DEFREQ, XFREQ, IRET)
         END IF
C                                       set DPARM defaults
      IF (DPARM(1).LT.0.0) DPARM(1) = 0
      IF (DPARM(2).EQ.0.0) DPARM(2) = -1.E8
      IF (DPARM(3).EQ.0.0) DPARM(3) = 1.E8
      IF (DPARM(4).LT.0.0) DPARM(4) = 0
      IF (DPARM(5).EQ.0.0) DPARM(5) = 1.E8
      IF (DPARM(6).LT.0.0) DPARM(6) = 0
      IF (DPARM(7).EQ.0.0) DPARM(7) = 1.E8
      IF (DPARM(8).EQ.0.0) DPARM(8) = -1.E8
      IF (DPARM(9).EQ.0.0) DPARM(9) = 1.E8
      DPARM(10) = 0.0
      NGOOD = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPIXIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1075 FORMAT ('ERROR',I5,' CREATING FILE TYPE ',A6)
      END
      SUBROUTINE FQGET (DISK, CNO, NF, FV, CV, CP, CI, CATBLK, DEFREQ,
     *   XFREQ, IRET)
C-----------------------------------------------------------------------
C   Gets the frequencies from the FQ table
C   Inputs:
C      DISK     I        disk
C      CNO      I        calatog number
C      NF       I        Number of frequencies
C      FV       D        Header ref frequency
C      CV       D        FQID axis ref value
C      CP       D        FQID axis ref pixel
C      CI       D        FQID axis increment
C      CATBLK   I(256)   old image header
C      DEFREQ   D        ref freq
C   Outputs:
C      XFREQ    D(*)     LOG10(freq/FV)
C      IERR     I        Error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, NF, CATBLK(*), IRET
      DOUBLE PRECISION FV, CV, DEFREQ, XFREQ(*)
      REAL      CP, CI
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(512), VER, LUN, IFQRNO, FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC), NUMIF, FQID, IFSIDE, IREC, NREC, I, MF
      DOUBLE PRECISION IFFREQ
      REAL      IFCHW, IFTBW
      CHARACTER BNDCOD*8
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       open
      VER = 1
      LUN = 20
      NUMIF = 1
      CALL FQINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, IFQRNO,
     *   FQKOLS, FQNUMV, NUMIF, IRET)
      IF (IRET.NE.0) GO TO 999
      NREC = BUFFER(5)
C                                       read
      DO 10 IREC = 1,NREC
         CALL TABFQ ('READ', BUFFER, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
         IF (IRET.NE.0) GO TO 20
         I = (FQID - CV) / CI + CP + 0.5
         XFREQ(I) = IFFREQ + FV
 10      CONTINUE
 20   CALL TABFQ ('CLOS', BUFFER, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *   FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IREC)
C                                       test
      MF = NF
      DO 30 I = 1,NF
         IF (XFREQ(I).GT.0.0D0) THEN
            MF = MF - 1
            XFREQ(I) = LOG10 (XFREQ(I)/DEFREQ)
            END IF
 30      CONTINUE
      IF (IRET.LE.0) IRET = -MF
      IF (MF.GT.0) THEN
         WRITE (MSGTXT,1030) MF
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('WARNING:',I3,' FREQUENCY PLANES NOT FOUND IN FQ FILE')
      END
      SUBROUTINE HIGET (DISK, CNO, NF, DEFREQ, XFREQ, IRET)
C-----------------------------------------------------------------------
C   HIGET tries to get the frequencies from the history file
C   Inputs:
C      DISK    I      Disk number
C      CNO     I      Catalog number
C      NF      I      Number of frequencies
C   Output
C      XFREQ   D(*)   Frequencies
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, NF, IRET
      DOUBLE PRECISION DEFREQ, XFREQ(*)
C
      INTEGER   IHLUN, NREC, IHPTR, HIBUFF(256), IBLK, ICARD, IP, MF,
     *   ICUR, IHIND, II
      CHARACTER LINE*72, CTYP*8
      DOUBLE PRECISION X
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MF = 0
      CALL DFILL (NF, 0.0D0, XFREQ)
C                                       open history file
      IHLUN = 27
C                                       Open history file.
      CALL HIINIT (3)
      CALL HIOPEN (IHLUN, DISK, CNO, HIBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL HILOCT ('SRCH', IHLUN, IHPTR, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Determine no. of hist. recs.
      NREC = HITAB(IHPTR+2)
      IHIND = HITAB(IHPTR+1)
      IBLK = 0
      ICARD = NHILPR
      DO 20 ICUR = 1,NREC
C                                       Read next buffer.
         ICARD = ICARD + 1
         IF (ICARD.GT.NHILPR) THEN
            IBLK = IBLK + 1
            ICARD = 1
            CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IRET)
            IF (IRET.NE.0) GO TO 100
            END IF
C                                       desired task?
         II = (ICARD-1) * NHIWPL + 5
         CALL H2CHR (72, 1, HIBUFF(II), LINE)
         IF (LINE(:12).EQ.'MCUBE COORD=') THEN
            READ (LINE,1000) X, CTYP, IP
C                                       test
            IF ((CTYP(:4).EQ.'FREQ') .AND. (IP.GT.0) .AND. (IP.LE.NF))
     *         THEN
               XFREQ(IP) = X
               MF = MF + 1
               WRITE (MSGTXT,1001) CTYP, X, IP
               CALL MSGWRT (3)
            ELSE
               WRITE (MSGTXT,1010) X, CTYP, IP
               CALL MSGWRT (7)
               END IF
            END IF
 20      CONTINUE
C                                       Close history file.
 100  CALL HICLOS (IHLUN, .FALSE., HIBUFF, II)
C                                       fill it all in??
      MF = NF
      DO 110 II = 1,NF
         IF (XFREQ(II).GT.0.0D0) THEN
            MF = MF - 1
            XFREQ(II) = LOG10 (XFREQ(II)/DEFREQ)
            END IF
 110     CONTINUE
      IF (IRET.LE.0) IRET = -MF
      IF (MF.GT.0) THEN
         WRITE (MSGTXT,1110) MF
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (12X,E13.5,5X,13X,A8,11X,I5)
 1001 FORMAT ('Found in MCUBE history ',A8,' F=',1PE13.5,' plane',I5)
 1010 FORMAT ('Coordinate mismatch',1PE13.5,' ''',A8,''' plane',I5)
 1110 FORMAT ('WARNING:',I3,
     *   ' FREQUENCY PLANES NOT FOUND IN HISTORY FILE')
      END
      SUBROUTINE SPIXDO (TBLNKD, IRET)
C-----------------------------------------------------------------------
C   SPIXDO sends image one row at a time to the moment fitting
C   routine and then writes the modified data.
C   Output: TBLNKD L    Answers contain blanked pixels?
C           IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER    IFILE*48, SFILE*48
      LOGICAL   TBLNKD
      INTEGER   IRET,  IROUND, LUNI, IERR, SIZE, NYI, NXI, WINI(4), BOI,
     *   BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2, I3, I4, I5,
     *   I6, I7, IPOS(7), CORN(7), BOTEMP, LIMIT, IBIND, INDI, I, LIM1,
     *   WINT(4), LUNT, INDT, NAXT(8), OBINDT, JBUFS3
      LOGICAL   T, F
      INCLUDE 'SPIXR.INC'
      REAL      DATA(MABFSS), BUFF3(MABFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNT /16,18/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      JBUFS3 = 2 * MABFSS
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Create scratch files.
C                                       For answers: temp scratch
      CALL COPY (7, CATBLK(KINAX), NAXT(2))
      NAXT(1) = 7
      I = CATBLK(KIDIM) + 1
      CALL MAPSIZ (I, NAXT, SIZE)
      CALL SCREAT (SIZE, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, SFILE, IERR)
      CALL ZOPEN (LUNT, INDT, SCRVOL(NSCR), SFILE, T, T, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1015) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
C                                       remember names switched
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINT(1) = 1
      WINT(2) = 1
      WINT(3) = NAXT(1)
      WINT(4) = NAXT(2)
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      CORN(7) = 1
      TBLNKD = .FALSE.
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4) = I4
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3) = I3
C                                       Init. files, first input.
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.EQ.0) GO TO 100
            WRITE (MSGTXT,1099) IRET
            GO TO 990
 100     BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
            END IF
C                                       Init answer file.
         I2 = CATBLK(KIDIM) + 1
         CALL COMOFF (I2, NAXT, CORN(3), BOTEMP, IRET)
         BOO = BOTEMP + 1
         CALL MINIT ('WRIT', LUNT, INDT, NAXT, NAXT(2), WINT, BUFF3,
     *      JBUFS3, BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'WRIT', IRET
            GO TO 990
            END IF
         DO 250 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       Copy to buffer.
            DO 165 I1 = 1,LIM1
               DATA(I1) = BUFF1(IBIND+I1-1)
 165           CONTINUE
C                                       Write.
            CALL MDISK ('WRIT', LUNT, INDT, BUFF3, OBINDT, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT', IRET
               GO TO 990
               END IF
C                                       Call DO1SPX
            IF (OPTYPE.NE.'CURV') THEN
               CALL DO1SPX (IPOS, DATA, BUFF3(OBINDT), IRET)
            ELSE
               CALL DO2SPX (IPOS, DATA, BUFF3(OBINDT), IRET)
               END IF
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1180) IRET
               IF (IRET.EQ.99) WRITE (MSGTXT,1181)
               GO TO 990
               END IF
C                                       Check max, min, blanking.
            IF (.NOT.TBLNKD) THEN
               LIMIT = OBINDT + WINT(3) - 1
               DO 215 I1 = OBINDT,LIMIT
                  TBLNKD = (TBLNKD) .OR. (BUFF3(I1).EQ.FBLANK)
 215              CONTINUE
               END IF
 250        CONTINUE
C                                       Flush buffers.
         CALL MDISK ('FINI', LUNT, INDT, BUFF3, OBINDT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) 'FINI', IRET
            GO TO 990
            END IF
 300     CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNT, INDT, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPIXDO: ERROR',I3,' OPENING INPUT FILE')
 1010 FORMAT ('SPIXDO: ERROR',I3,' CREATING ANSWER SCRATCH FILE')
 1015 FORMAT ('SPIXDO: ERROR',I3,' OPENING ANSWER SCRATCH FILE')
 1099 FORMAT ('SPIXDO: COMOF3 ERROR',I3)
 1100 FORMAT ('SPIXDO: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('SPIXDO: ',A4,' ERROR',I3)
 1180 FORMAT ('SPIXDO: DOnSPX ERROR',I3)
 1181 FORMAT ('Quitting at user request')
      END
      SUBROUTINE SPIHED (IRET)
C-----------------------------------------------------------------------
C   SPIHED modifies the new image header for the subimaging and for
C   replacing the first axis with Gaussian components.
C   Input:
C      CATBLK(256)    I     Output catalog header, also CATR, CATD,
C                           CATH
C      CATOLD(256)    I     Input catalog header, also CATOR, CATOD,
C                           CATOH
C   Output:
C      CATBLK(256)    I     Modified output catalog header.
C      IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CHTM12*12
      INCLUDE 'SPIXR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set axes in output CATBLK.
      CALL SUBHDR (BLC, TRC, 1.0, 1.0)
C                                       Check input axes
      CALL H2CHR (8, 1, CATH(KHCTP), CHTM12)
      IF ((CHTM12(:4).NE.'FREQ') .AND. (CHTM12(:8).NE.'SEQ.NUM.') .AND.
     *   (CHTM12(:8).NE.'FQID')) THEN
         MSGTXT = 'FIRST AXIS NOT FREQUENCY, FQID, OR SEQ.NUM.'
         CALL MSGWRT (8)
         IRET = 8
         END IF
C
 999  RETURN
      END
      SUBROUTINE DO1SPX (IPOS, IDATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   DO1SPX fits moments to a row of an image and returns the
C   answers in RESULT.
C   Inputs:
C      IPOS(7)   I      BLC (input image) of first value in DATA
C      IDATA     R(*)   Input data (floated and scaled)
C   Values from commons:
C      FCUT      R      Flux cutoff
C      FBLANK    R      Value of blanked pixel.
C      CATOLD    I      Input catalog header (also CATOR, CATOD)
C   Output:
C      RESULT(*) R      Output row (count, 4 moments)
C      IRET      I      Return code   0 => OK
C                               >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   IRET, IPOS(7)
      REAL      IDATA(*), RESULT(*)
C
      INTEGER   INPTS, I
      REAL      LFCUT, V , TMIN, TMAX
      DOUBLE PRECISION X, Y, SXX, SX, SY, SXY, SN, DET, VM, VB, SDD
      LOGICAL   FAIL
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'SPIXR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Not last call
      IF (IPOS(1).GE.0) THEN
         LFCUT = FCUT
         LFCUT = MAX (1.E-10, LFCUT)
         INPTS = TRC(1) - BLC(1) + 1.01
         SXX = 0.0
         SX  = 0.0
         SXY = 0.0
         SY  = 0.0
         SN  = 0.0
         TMIN = 1.E8
         TMAX = -TMIN
         DO 20 I = 1,INPTS
            V = IDATA(I)
            IF ((V.GT.0.0) .AND. (V.NE.FBLANK)) THEN
               IF (PBPARM(1).GT.0.0) THEN
                  CALL SPIXPB (I, IPOS, LFCUT, IRET)
                  IF (IRET.NE.0) GO TO 20
                  END IF
               IF (V.GE.LFCUT) THEN
                  TMIN = MIN (TMIN, V)
                  TMAX = MAX (TMAX, V)
                  Y = LOG10 (V)
                  X = XFREQ(I)
                  SX = SX + X
                  SY = SY + Y
                  SXX = SXX + X * X
                  SXY = SXY + X * Y
                  SN = SN + 1.0
                  END IF
               END IF
 20         CONTINUE
         DET = SN * SXX - SX * SX
         FAIL = (DET.LE.0.0) .OR. (SN.LE.1.5)
         IF (.NOT.FAIL) THEN
            VB = (SXX * SY - SX * SXY) / DET
            VM = (SN * SXY - SX * SY) / DET
C                                       find overall sigma**2
            SDD = 0.0
            DO 30 I = 1,INPTS
               Y = IDATA(I)
               IF ((Y.GT.0.0) .AND. (Y.NE.FBLANK)) THEN
                  IF (PBPARM(1).GT.0.0) THEN
                     CALL SPIXPB (I, IPOS, LFCUT, IRET)
                     IF (IRET.NE.0) GO TO 30
                     END IF
                  IF (Y.GE.LFCUT) THEN
                     Y = LOG10 (Y)
                     X = XFREQ(I)
                     SDD = SDD + (VM * X + VB - Y)**2
                     END IF
                  END IF
 30            CONTINUE
            SDD = SDD / SN
            RESULT(1) = VB
            RESULT(4) = VM
            RESULT(3) = SN
            RESULT(2) = SQRT (SDD * SXX / DET)
            RESULT(5) = SQRT (SN * SDD / DET)
            RESULT(6) = 0.0
            RESULT(7) = 0.0
C                                       but want T not log T
            RESULT(1) = 10.0 ** (RESULT(1))
            RESULT(2) = RESULT(1) * RESULT(2)
C                                       test results
            IF (RESULT(3).LT.DPARM(1)) FAIL = .TRUE.
            IF (RESULT(4).LT.DPARM(2)) FAIL = .TRUE.
            IF (RESULT(4).GT.DPARM(3)) FAIL = .TRUE.
            IF (TMIN.GT.0.0) THEN
               IF (RESULT(1)/TMIN.LT.DPARM(4)) FAIL = .TRUE.
               END IF
            IF (RESULT(1)/TMAX.GT.DPARM(5)) FAIL = .TRUE.
            IF (RESULT(1).LT.DPARM(6)) FAIL = .TRUE.
            IF (RESULT(1).GT.DPARM(7)) FAIL = .TRUE.
            IF (FAIL) DPARM(10) = DPARM(10) + 1.0
            END IF
C                                       Max / Min
         IF (.NOT.FAIL) THEN
            NGOOD = NGOOD + 1
            DO 40 I = 1,7
               PMAX(I) = MAX (PMAX(I), RESULT(I))
               PMIN(I) = MIN (PMIN(I), RESULT(I))
 40            CONTINUE
         ELSE
            CALL RFILL (7, FBLANK, RESULT)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE DO2SPX (IPOS, IDATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   DO1SPX fits spectral index plus curvature to a row of an image and
C   returns the answers in RESULT.
C   Inputs:
C      IPOS(7)   I      BLC (input image) of first value in DATA
C      IDATA     R(*)   Input data (floated and scaled)
C   Values from commons:
C      FCUT      R      Flux cutoff
C      FBLANK    R      Value of blanked pixel.
C      CATOLD    I      Input catalog header (also CATOR, CATOD)
C   Output:
C      RESULT(*) R      Output row (count, 4 moments)
C      IRET      I      Return code   0 => OK
C                               >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   IRET, IPOS(7)
      REAL      IDATA(*), RESULT(*)
C
      INTEGER   INPTS, I
      REAL      LFCUT, V, TMAX, TMIN
      DOUBLE PRECISION X, Y, SXXXX, SXXX, SXX, SX, SY, SXY, SXXY, SN,
     *   DET, VM, VB, VC, SDD, AB, AC, AM, BB, BC, BM, CB, CC, CM
      LOGICAL   FAIL
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'SPIXR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Not last call
      IF (IPOS(1).GE.0) THEN
         LFCUT = FCUT
         LFCUT = MAX (1.E-10, LFCUT)
         INPTS = TRC(1) - BLC(1) + 1.01
         SXXXX = 0.0
         SXXX = 0.0
         SXX = 0.0
         SX = 0.0
         SXXY = 0.0
         SXY = 0.0
         SY = 0.0
         SN = 0.0
         TMIN = 1.E8
         TMAX = -TMIN
         DO 20 I = 1,INPTS
            V = IDATA(I)
            IF ((V.GT.0.0) .AND. (V.NE.FBLANK)) THEN
               IF (PBPARM(1).GT.0.0) THEN
                  CALL SPIXPB (I, IPOS, LFCUT, IRET)
                  IF (IRET.NE.0) GO TO 20
                  END IF
               IF (V.GE.LFCUT) THEN
                  TMIN = MIN (TMIN, V)
                  TMAX = MAX (TMAX, V)
                  Y = LOG10 (V)
                  X = XFREQ(I)
                  SX = SX + X
                  SY = SY + Y
                  SXX   = SXX + X * X
                  SXXXX = SXXXX + X * X * X * X
                  SXXX  = SXXX + X * X * X
                  SXXY = SXXY + X * X * Y
                  SXY = SXY + X * Y
                  SN = SN + 1.0
                  END IF
               END IF
 20         CONTINUE
         DET = SN*SXXXX*SXX - SXXXX*SX*SX - SN*SXXX*SXXX +
     *      2.0*SXXX*SXX*SX  - SXX*SXX*SXX
         FAIL = (DET.LE.0.0) .OR. (SN.LE.2.5)
         IF (.NOT.FAIL) THEN
C                                       const (log brightness)
            AB = (SXXX*SX - SXX*SXX) / DET
            BB = (SXXX*SXX - SXXXX*SX) / DET
            CB = (SXXXX*SXX - SXXX*SXXX) / DET
            VB = (SXXY * AB + SXY * BB + SY * CB)
C                                       slope (spectral index)
            AM = (SXX*SX - SN*SXXX) / DET
            BM = (SN*SXXXX - SXX*SXX) / DET
            CM = (SXXX*SXX - SXXXX*SX) / DET
            VM = (SXXY * AM + SXY * BM + SY * CM)
C                                       curvature
            AC = (SN*SXX - SX*SX) / DET
            BC = (SXX*SX - SN*SXXX) / DET
            CC = (SXXX*SX - SXX*SXX) / DET
            VC = (SXXY * AC + SXY * BC + SY * CC)
C                                       find overall sigma**2
            SDD = 0.0
            DO 30 I = 1,INPTS
               Y = IDATA(I)
               IF ((Y.GT.0.0) .AND. (Y.NE.FBLANK)) THEN
                  IF (PBPARM(1).GT.0.0) THEN
                     CALL SPIXPB (I, IPOS, LFCUT, IRET)
                     IF (IRET.NE.0) GO TO 30
                     END IF
                  IF (Y.GE.LFCUT) THEN
                     Y = LOG10 (Y)
                     X = XFREQ(I)
                     SDD = SDD + (VC * X * X + VM * X + VB - Y)**2
                     END IF
                  END IF
 30            CONTINUE
            SDD = SDD / SN
            RESULT(1) = VB
            RESULT(4) = VM
            RESULT(3) = SN
            RESULT(6) = VC
C                                       error bars
            X = AB * AB * SXXXX + BB * BB * SXX + CB * CB +
     *         2. * (AB*BB*SXXX + AB*CB*SXX + BB*CB*SX)
            X = ABS (X)
            RESULT(2) = SQRT (SDD * X)
            X = AM * AM * SXXXX + BM * BM * SXX + CM * CM +
     *         2. * (AM*BM*SXXX + AM*CM*SXX + BM*CM*SX)
            X = ABS (X)
            RESULT(5) = SQRT (SDD * X)
            X = AC * AC * SXXXX + BC * BC * SXX + CC * CC +
     *         2. * (AC*BC*SXXX + AC*CC*SXX + BC*CC*SX)
            X = ABS (X)
            RESULT(7) = SQRT (SDD * X)
C                                       but want T not log T
            RESULT(1) = 10.0 ** (RESULT(1))
            RESULT(2) = RESULT(1) * RESULT(2)
C                                       test results
            IF (RESULT(3).LT.DPARM(1)) FAIL = .TRUE.
            IF (RESULT(4).LT.DPARM(2)) FAIL = .TRUE.
            IF (RESULT(4).GT.DPARM(3)) FAIL = .TRUE.
            IF (TMIN.GT.0.0) THEN
               IF (RESULT(1)/TMIN.LT.DPARM(4)) FAIL = .TRUE.
               END IF
            IF (RESULT(1)/TMAX.GT.DPARM(5)) FAIL = .TRUE.
            IF (RESULT(1).LT.DPARM(6)) FAIL = .TRUE.
            IF (RESULT(1).GT.DPARM(7)) FAIL = .TRUE.
            IF (RESULT(6).LT.DPARM(8)) FAIL = .TRUE.
            IF (RESULT(6).GT.DPARM(9)) FAIL = .TRUE.
            IF (FAIL) DPARM(10) = DPARM(10) + 1.0
            END IF
C                                       Max / Min
         IF (.NOT.FAIL) THEN
            NGOOD = NGOOD + 1
            DO 40 I = 1,7
               PMAX(I) = MAX (PMAX(I), RESULT(I))
               PMIN(I) = MIN (PMIN(I), RESULT(I))
 40            CONTINUE
         ELSE
            CALL RFILL (7, FBLANK, RESULT)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE SPIXPB (IX, IPOS, FCC, IRET)
C-----------------------------------------------------------------------
C   SPIXPB computes the primary beam correction at the current pixel and
C   adjusts FCUT and ICUT for it.
C   Inputs:
C      IX     I      X pixel position
C      IPOS   I(7)   ?,Y,Z,... pixels
C   Output:
C      FCC    R      FCUT adjusted
C      IRET   I      0 - okay, 1 outside the usable beam
C-----------------------------------------------------------------------
      INTEGER   IX, IPOS(*), IRET
      REAL      FCC
C
      INTEGER   LPOS(7), LF, LR, LD, CATSAV(256), LY
      DOUBLE PRECISION DX, DY, DT, X, Y, LAMBDA, ANGLE
      REAL      PBCORF
      CHARACTER ARRAY*8
      LOGICAL   OUTSID
      INCLUDE 'SPIXR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE LF, LR, LD, LY, LAMBDA, ANGLE
      DATA LF, LR, LD, LY /3*0,300000/
C-----------------------------------------------------------------------
      CALL COPY (6, IPOS(2), LPOS(2))
      LPOS(1) = IX
      IRET = 0
      CALL H2CHR (8, 1, CATH(KHTEL), ARRAY)
C                                       init common each plane
      IF (LPOS(2).LT.LY) THEN
        CALL COPY (256, CATBLK, CATSAV)
        LOCNUM = 1
        CALL COPY (256, CATOLD, CATBLK)
        CALL SETLOC (LPOS(3), .FALSE.)
        END IF
      LY = LPOS(2)
C                                       new frequency
      IF (LF.NE.LPOS(1)) THEN
         LF = LPOS(1)
         LAMBDA = XFREQ(LF) + 9.0D0
         LAMBDA = 10.0D0**(LAMBDA)
         LAMBDA = VELITE / LAMBDA
         END IF
C                                       new coordinate
      IF ((LD.NE.LPOS(DAX)) .OR. (LR.NE.LPOS(RAX))) THEN
         DX = (LPOS(RAX) - RR) * RI
         DY = (LPOS(DAX) - DR) * DI
         DT = DX * COS (MROT) - DY * SIN (MROT)
         DY = DY * COS (MROT) + DX * SIN (MROT)
         DX = DT
         CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RA0, DE0, DX, DY,
     *      X, Y, IRET)
         IF (IRET.NE.0) THEN
            LD = -10
            GO TO 999
            END IF
         DT = SIN (DE0) * SIN (Y) + COS (DE0) * COS (Y) * COS (RA0-X)
         DT = MIN (1.0D0, DT)
         DT = MAX (-1.0D0, DT)
         ANGLE = RAD2DG * ACOS (DT)
         LD = LPOS(DAX)
         LR = LPOS(RAX)
         END IF
C                                       primary beam
      CALL PBCALC (ANGLE, LAMBDA, ARRAY, PBPARM(2), PBCORF, OUTSID)
      IF ((OUTSID) .OR. (PBCORF.LE.0.0) .OR. (PBCORF.LT.PBPARM(1))) THEN
         IRET = 1
      ELSE
         FCC = FCUT / PBCORF
         END IF
C
 999  RETURN
      END
      SUBROUTINE SPIXOU (BLNKD, IRET)
C-----------------------------------------------------------------------
C   SPIXOU creates and fills (via PSCALE) the individual moment maps.
C   It calls SPIXHI for history info for all images.
C   Inputs: BLNKD   L      Are any parameters blanked?
C   Output: IRET    I      0 => ok,  4 => real trouble.
C-----------------------------------------------------------------------
      LOGICAL   BLNKD
      INTEGER   IRET
C
      CHARACTER CLAOUT*6, SEQTYP*6
      INTEGER   NXO, NYO, WINI(4), WINO(4), IERR, IG, IOFF, IP, NP
      INCLUDE 'SPIXR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       loop limits etc.
      WINI(1) = 1
      WINI(2) = 1
      WINI(3) = 7
      WINO(1) = 1
      WINO(2) = 1
      NP = 5
      IF (OPTYPE.EQ.'CURV') NP = 7
C                                       Output Gaussian parms
      DO 40 IG = 1,NP
         IP = NCFILE - NP + IG
         NEWCNO = FCNO(IP)
         DISKO = FVOL(IP)
         CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.6)) THEN
            WRITE (MSGTXT,1000) IERR, IP
            GO TO 990
            END IF
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), SEQTYP)
         IOFF = IG
         WRITE (MSGTXT,1020) SEQTYP
         CALL MSGWRT (1)
         SEQOUT = CATBLK(KIIMS)
         CALL H2CHR (12, KHIMNO, CATH(KHIMNO), NAMOUT)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT)
         NXO = CATBLK(KINAX)
         NYO = CATBLK(KINAX+1)
         WINI(4) = NXO
         WINO(3) = NXO
         WINO(4) = NYO
C                                       Fill image
         CALL PSCALE (IOFF, NSCR, WINI, NEWCNO, DISKO, WINO, JBUFSZ,
     *      PMAX, PMIN, BLNKD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1021) IERR, SEQTYP
            GO TO 990
            END IF
C                                       History, close
         CALL SPIXHI (IP, CLAOUT)
 40      CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IRET = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' RECOVERING HEADER NUMBER',I5)
 1020 FORMAT ('Begin writing file of type ',A6)
 1021 FORMAT ('ERROR',I5,' MOVING DATA TO FILE TYPE ',A6)
      END
      SUBROUTINE PSCALE (IOFF, ISCR, WINI, NEWCNO, DISKO, WINO,
     *   JBUFSZ, PMAX, PMIN, BLNKD, IERR)
C-----------------------------------------------------------------------
C   PSCALE reads a floating point map file extracting one point per row
C   and writes an image out.
C   Inputs:
C      IOFF     I      Pixel in row to extract (1-rel)
C      ISCR     I      Scratch file number in CFIL common
C      WINI     I(4)   Input window
C      NEWCNO   I      Output catalog number
C      DISKO    I      Output disk number
C      WINO     I(4)   Output Window
C      JBUFSZ   I      Buffer size in bytes
C      PMAX     R(*)   Max values by columns
C      PMIN     R(*)   Min values by columns
C      BLNKD    L      Image is blanked
C   Output:
C      IERR     I      0 -> ok, else IO error
C      CATBLK in common: change max/min and scaling and blanking
C      Buffers in common
C-----------------------------------------------------------------------
      INTEGER   IOFF, ISCR, WINI(4), NEWCNO, DISKO, WINO(4), JBUFSZ,
     *   IERR
      REAL      PMAX(*), PMIN(*)
      LOGICAL   BLNKD
C
      CHARACTER PHNAME*48
      INTEGER   NXO, L3, L4, L5, L6, L7, I2, I3, I4, I5, I6, I7, J,
     *   LUNI, LUNO, INDI, INDO, IPOS(8), NAXT(8), INDIM, BOTEMP,
     *   OBIND, IBIND, L, JERR
      LOGICAL   T
      INCLUDE 'XMBUFRS'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16, 17/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Set maxima, clear blanking
      CATR(KRDMX) = PMAX(IOFF)
      CATR(KRDMN) = PMIN(IOFF)
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       loop limits
      L3 = CATBLK(KINAX+1)
      L4 = CATBLK(KINAX+2)
      L5 = CATBLK(KINAX+3)
      L6 = CATBLK(KINAX+4)
      L7 = CATBLK(KINAX+5)
      NXO = WINO(3)
C                                       Open files
      CALL ZPHFIL ('SC', SCRVOL(ISCR), SCRCNO(ISCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUNI, INDI, SCRVOL(ISCR), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, PHNAME, IERR)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Prepare to read
      IPOS(8) = 1
      CALL COPY (7, CATBLK(KINAX), NAXT(2))
      NAXT(1) = WINI(3)
      INDIM = CATBLK(KIDIM) + 1
C                                       loop
      DO 700 I7 = 1,L7
         IPOS(7) = I7
         DO 600 I6 = 1,L6
            IPOS(6) = I6
            DO 500 I5 = 1,L5
               IPOS(5) = I5
               DO 400 I4 = 1,L4
                  IPOS(4) = I4
C                                       Init output
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(4), BOTEMP, IERR)
      BOTEMP = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, WINO(3), WINO(4), WINO, BUFF2,
     *   JBUFSZ, BOTEMP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 970
         END IF
      DO 300 I3 = 1,L3
         IPOS(3) = I3
         CALL COMOFF (INDIM, NAXT, IPOS(3), BOTEMP, IERR)
         BOTEMP = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, WINI(3), WINI(4), WINI, BUFF1,
     *      JBUFSZ, BOTEMP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1110) IERR
            GO TO 970
            END IF
C                                       Init a write
         CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1120) 'WRIT', IERR
            GO TO 970
            END IF
C                                       Loop thru input plane
         DO 200 I2 = 1,NXO
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IERR
               GO TO 970
               END IF
            J = IBIND + IOFF - 1
            L = OBIND + I2 - 1
            BUFF2(L) = BUFF1(J)
 200        CONTINUE
 300     CONTINUE
C                                       Flush output plane
      CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) 'FINI', IERR
         GO TO 970
         END IF
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
      GO TO 975
C                                       Close down (error)
 970  CALL MSGWRT (8)
C                                       Close files
 975  CALL ZCLOSE (LUNO, INDO, JERR)
 980  CALL ZCLOSE (LUNI, INDI, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('PSCALE: ERROR',I5,' OPENING SCRATCH FILE')
 1035 FORMAT ('PSCALE: ERROR',I5,' OPENING MAP FILE')
 1100 FORMAT ('PSCALE: ERROR',I5,' ON INIT MAP FILE')
 1110 FORMAT ('PSCALE: ERROR',I5,' ON INIT SCRATCH FILE')
 1120 FORMAT ('PSCALE: ',A4,' ERROR',I5)
      END
      SUBROUTINE SPIXHI (NCN, CLAOUT)
C-----------------------------------------------------------------------
C   SPIXHI copies and updates history file.
C   Inputs:
C      NCN      I     Index in catlgd files common
C      CLAOUT   C*6   Output map CLASS
C-----------------------------------------------------------------------
      CHARACTER CLAOUT*6
      INTEGER   NCN
C
      CHARACTER LABEL*8, DCOM(9)*14, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I
      REAL      DMIN(9)
      LOGICAL   T
      INCLUDE 'SPIXR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA DMIN / 0.0, -1.E7, -1.E7, 0.0, 0.0, 0.0, 0.0, -1.E7, -1.E7/
      DATA DCOM /'Num pix < D(1)', 'Spix < D(2)', 'Spix > D(3)',
     *   'T/Tmin < D(4)', 'T/Tmax < D(5)', 'T < D(6)',
     *   'T > D(7)', 'Curv < D(8)', 'Curv > D(9)'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRTCH(257), SCRTCH, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 50
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       other Parms
      CALL H2CHR (8, 1, CATOH(KHBUN), LABEL)
      WRITE (HILINE,2002) TSKNAM, FCUT, LABEL
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Old axis 1
      CALL H2CHR (8, 1, CATOH(KHCTP), LABEL)
      WRITE (HILINE,2020) TSKNAM, LABEL
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,2021) TSKNAM, CATOLD(KINAX)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,2022) TSKNAM, CATOR(KRCRP)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,2023) TSKNAM, CATOR(KRCIC)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,2024) TSKNAM, CATOD(KDCRV)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       ref freq
      WRITE (HILINE,2025) TSKNAM, REFREQ
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Cutoffs
      DO 30 I = 1,9
         IF ((DPARM(I).GT.DMIN(I)) .AND. (DPARM(I).LT.1.E7)) THEN
            WRITE (HILINE,2030) TSKNAM, I, DPARM(I), DCOM(I)
            CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 50
            END IF
 30      CONTINUE
C                                       blanked
      IF (DPARM(10).GT.0.0) THEN
         I = DPARM(10)
         WRITE (HILINE,2031) TSKNAM, I
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         MSGTXT = HILINE(9:)
         IF (NCN.EQ.NCFILE) CALL MSGWRT (4)
         END IF
C                                       good
      WRITE (HILINE,2032) TSKNAM, NGOOD
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      MSGTXT = HILINE(9:)
      IF (NCN.EQ.NCFILE) CALL MSGWRT (4)
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                        Update CATBLK and close
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', SCRTCH, IERR)
      FRW(NCN) = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPIXHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,'FLUX =',1PE12.4,14X,'/ Use only t > flux ',A8)
 2020 FORMAT (A6,'CTYPE1  = ''',A8,'''',12X,'/ Old axis 1')
 2021 FORMAT (A6,'NAXIS1  = ',I6,16X,'/ Old axis 1')
 2022 FORMAT (A6,'CRPIX1  = ',F9.3,13X,'/ Old axis 1')
 2023 FORMAT (A6,'CDELT1  = ',1PE13.5,9X,'/ Old axis 1')
 2024 FORMAT (A6,'CRVAL1  = ',1PE18.10,4X,'/ Old axis 1')
 2025 FORMAT (A6,'REFREQ = ',F9.4,4X,'/ reference frequency GHz')
 2030 FORMAT (A6,'DPARM(',I1,')= ',F12.3,4X,
     *   '/ Flag solution if ',A)
 2031 FORMAT (A6,'/ Number possible solutions flagged=',I10)
 2032 FORMAT (A6,'/ Number good solutions=',I10)
      END
