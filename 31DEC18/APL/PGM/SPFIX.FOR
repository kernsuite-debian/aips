LOCAL INCLUDE 'XMBUFRS'
      INCLUDE 'INCS:PMAD.INC'
      REAL     BUFF1(MABFSS), BUFF2(MABFSS), SCRTCH(512)
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
LOCAL END
LOCAL INCLUDE 'SPFIX.INC'
      INCLUDE 'XMBUFRS'
      CHARACTER NAMEIN*12, CLAIN*6, NAMEI2*12, NAMOUT*12, CLASOU*6,
     *   OPTYPE*4, OPCODE*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XNAMOU(3), XCLAOU(2),
     *   XOPTYP, XOPCOD, CATOH(256), CAT2H(256)
      REAL      XSEQIN, XDISKI, BLC(7), TRC(7), XSEQI2, XDISK2, XSEQO,
     *   XDISKO
      DOUBLE PRECISION CATOD(128), FV, RV, DV, RA0, DE0, XFREQ(MAXIMG),
     *   DEFREQ, CAT2D(128)
      REAL      CATOR(256), PMIN(7), PMAX(7), FI, RI, DI, FR, RR, DR,
     *   MROT, CAT2R(256)
      INTEGER   CATOLD(256), CAT2(256), SEQIN, SEQI2, SEQOUT, DISKIN,
     *   DISKI2, DISKO, NEWCNO, CNO2, OLDCNO, JBUFSZ, FAX, RAX, DAX,
     *   NGOOD
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, BLC, TRC, XNAME2,
     *   XSEQI2, XDISK2, XNAMOU, XCLAOU, XSEQO, XDISKO, XOPTYP, XOPCOD
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMEI2, NAMOUT, CLASOU, OPTYPE,
     *   OPCODE
      COMMON /PARMS/ CATOLD, CAT2, FV, RV, DV, RA0, DE0, XFREQ, DEFREQ,
     *   PMAX, PMIN, SEQIN, SEQI2, SEQOUT, DISKIN, DISKI2, DISKO,
     *   NEWCNO, OLDCNO, CNO2, JBUFSZ, FI, RI, DI, FR, RR, DR, FAX, RAX,
     *   DAX, MROT, NGOOD
      EQUIVALENCE (CATOLD, CATOR, CATOH, CATOD)
      EQUIVALENCE (CAT2, CAT2R, CAT2H, CAT2D)
LOCAL END
      PROGRAM SPFIX
C-----------------------------------------------------------------------
C! SPFIX makes model cube from input/output of SPIXR
C# Map-util Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2018
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
C   SPFIX fits 1-dimensional spectral inices to rows of an image.  It
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
      INTEGER   IRET, NX, NY, NWORDS, NP
      REAL      MODATA(2)
      LONGINT   PMODAT
      INCLUDE 'SPFIX.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'SPFIX'/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SPFIXI (PRGM, IRET)
      IF (IRET.EQ.0) THEN
         NP = 2
         IF (OPTYPE.EQ.'CURV') NP = 3
         NX = CAT2(KINAX)
         NY = CAT2(KINAX+1)
         NWORDS = (NX * NY * NP - 1) / 1024 + 6
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MODATA, PMODAT, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'FAILED TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            END IF
         END IF
      IF (IRET.EQ.0) CALL SPFIXR (NX, NY, NP, MODATA(1+PMODAT), IRET)
      IF (IRET.EQ.0) THEN
         IF (OPCODE.EQ.'DIFF') THEN
            CALL SPFIXD (NX, NY, NP, MODATA(1+PMODAT), IRET)
         ELSE
            CALL SPFIXM (NX, NY, NP, MODATA(1+PMODAT), IRET)
            END IF
         END IF
      IF (IRET.EQ.0) CALL SPFIXH
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE SPFIXI (PRGN, IRET)
C-----------------------------------------------------------------------
C   SPFIXI gets input parameters for SPFIX.
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
      CHARACTER STAT*4, PRGN*6, OTYPE*8, CLAOUT*6, MTYPE*2, CLAI2*6
      INTEGER   IRET, IPT, I, IERR, NPARM, IROUND, IG, NAX
      DOUBLE PRECISION C1CRV
      REAL      C1CRP, C1CIC
      INCLUDE 'SPFIX.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
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
      NPARM = 35
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAMEI2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLASOU)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      SEQIN = IROUND (XSEQIN)
      SEQI2 = IROUND (XSEQI2)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKI2 = IROUND (XDISK2)
      DISKO = IROUND (XDISKO)
      IF (OPTYPE.NE.'CURV') OPTYPE = ' '
      IF (OPCODE.NE.'DIFF') OPCODE = 'MODL'
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
C                                       Read CATBLK do not mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING INPUT FILE HEADER '
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
C                                       find FREQ axis
      FAX = 0
      NAX = CATOLD(KIDIM)
      DO 20 I = 1,NAX
         CALL H2CHR (8, 1, CATOH(KHCTP+2*(I-1)), OTYPE)
         IF (OTYPE(:4).EQ.'FREQ') THEN
            FAX = I
            FV = CATOD(KDCRV+I-1)
            FI = CATOR(KRCIC+I-1)
            FR = CATOR(KRCRP+I-1)
            END IF
 20      CONTINUE
      IF (FAX.LE.0) THEN
         MSGTXT = 'NO FREQUENCY AXIS FOUND: CANNOT FUNCTION'
         GO TO 990
         END IF
C                                       get model Brightness
      CNO2 = 1
      CLAI2 = 'B TEMP'
      CALL CATDIR ('SRCH', DISKI2, CNO2, NAMEI2, CLAI2, SEQI2, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEI2, CLAI2, SEQI2, DISKI2,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK, mark 'READ'.
      CALL CATIO ('READ', DISKI2, CNO2, CAT2, 'REST', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING B TEMP IMAGE HEADER'
         GO TO 990
         END IF
C                                       find ref freq
      NAX = CAT2(KIDIM)
      DEFREQ = 0.0D0
      DO 25 I = 1,NAX
         CALL H2CHR (8, 1, CAT2H(KHCTP+2*(I-1)), OTYPE)
         IF (OTYPE(:4).EQ.'FREQ') THEN
            DEFREQ = CAT2D(KDCRV+I-1)
            END IF
 25      CONTINUE
      IF (DEFREQ.LE.0.0D0) THEN
         MSGTXT = 'NO REFERENCE FREQUENCY IN B TEMP IMAGE'
         GO TO 990
         END IF
      NAX = TRC(2) - BLC(2) + 1.01
      I = TRC(3) - BLC(3) + 1.01
      IF ((CAT2(KINAX).NE.NAX) .OR. (CAT2(KINAX+1).NE.I)) THEN
         MSGTXT = 'BLC/TRC DOES NOT MATCH B TEMP IMAGE'
         GO TO 990
         END IF
C                                       get frequencies
      CALL H2CHR (8, 1, CATOH(KHCTP), OTYPE)
      NAX = CATOLD(KINAX)
      IF (OTYPE(:4).EQ.'FREQ') THEN
         DO 30 I = 1,NAX
            XFREQ(I) = FV + FI * (I - FR)
            XFREQ(I) = LOG10 (XFREQ(I)/DEFREQ)
 30         CONTINUE
      ELSE IF (OTYPE.EQ.'SEQ.NUM.') THEN
         CALL HIGET (DISKIN, OLDCNO, NAX, DEFREQ, XFREQ, IRET)
      ELSE
         C1CRV = CATOD(KDCRV)
         C1CRP = CATOR(KRCRP)
         C1CIC = CATOR(KRCIC)
         CALL FQGET (DISKIN, OLDCNO, NAX, FV, C1CRV, C1CRP, C1CIC,
     *      CATOLD, DEFREQ, XFREQ, IRET)
         END IF
C                                       correct XFREQ for BLC(1)
      IPT = BLC(1) + 0.1
      IF (IPT.GT.1) THEN
         IPT = IPT - 1
         IG = TRC(1) - BLC(1) + 1.01
         DO 35 I = 1,IG
            XFREQ(I) = XFREQ(I+IPT)
 35         CONTINUE
         END IF
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLASOU,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASOU, KHIMCO, CATH(KHIMN))
      CATBLK(KIIMS) = SEQOUT
C                                       create output file
C                                       Create
      DISKO = XDISKO + 0.01
      NEWCNO = 1
      CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING OUTPUT IMAGE FILE'
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
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPFIXI: ERROR',I3,' ON ',A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
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
      SUBROUTINE SPFIXR (NX, NY, NP, MODELS, IRET)
C-----------------------------------------------------------------------
C   SPFIXR reads the 2 or 3 model images into ram
C   Inputs:
C      NX       I      Number X pixels
C      NY       I      Number Y pixels
C      NP       I      Number model images
C   Output:
C      MODELS   R(*)   Models (NX,NY,NP)
C      IRET     I      > 0 -> quit
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NP, IRET
      REAL      MODELS(NX,NY,*)
C
      INCLUDE 'SPFIX.INC'
      INTEGER   IY, IP, LUNI, WINI(4), BOTEMP, BOI, IBIND, INDI,
     *   DEPTH(5)
      CHARACTER CLAS2(3)*6, CLAI2*6, STAT*4, MTYPE*2, IFILE*48
      LOGICAL   F, T
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA CLAS2 /'B TEMP', 'SPIX', 'SPCU'/
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
      DATA MTYPE /'MA'/
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      WINI(1) = 1
      WINI(2) = 1
      WINI(3) = NX
      WINI(4) = NY
      DO 100 IP = 1,NP
         CNO2 = 1
         CLAI2 = CLAS2(IP)
         CALL CATDIR ('SRCH', DISKI2, CNO2, NAMEI2, CLAI2, SEQI2, MTYPE,
     *      NLUSER, STAT, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET, NAMEI2, CLAI2, SEQI2, DISKI2,
     *         NLUSER
            GO TO 990
            END IF
C                                       Read CATBLK, mark 'READ'.
         CALL CATIO ('READ', DISKI2, CNO2, CAT2, 'READ', SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING ' // CLAI2 //
     *         ' IMAGE HEADER'
            GO TO 990
            END IF
C                                       Record the creation
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKI2
         FCNO(NCFILE) = CNO2
         FRW(NCFILE) = 0
C                                       Open and init for read
         CALL ZPHFIL ('MA', DISKI2, CNO2, 1, IFILE, IRET)
         CALL ZOPEN (LUNI, INDI, DISKI2, IFILE, T, F, T, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING ' // CLAI2 //
     *         ' IMAGE FILE'
            GO TO 990
            END IF
C                                       Init. files, first input.
         CALL COMOFF (CAT2(KIDIM), CAT2(KINAX), DEPTH, BOTEMP, IRET)
         BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, NX, NY, WINI, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ ' // CLAI2 //
     *         ' IMAGE FILE'
            GO TO 990
            END IF
         DO 30 IY = 1,NY
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING ' // CLAI2 //
     *            ' IMAGE FILE'
               GO TO 990
               END IF
            CALL RCOPY (NX, BUFF1(IBIND), MODELS(1,IY,IP))
 30         CONTINUE
         CALL ZCLOSE (LUNI, INDI, IRET)
 100     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPFIXR: ERROR',I3,' ON ',A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
      END
      SUBROUTINE SPFIXM (NX, NY, NP, MODELS, IRET)
C-----------------------------------------------------------------------
C   SPFIXR writes the model cube out
C   Inputs:
C      NX       I      Number X pixels
C      NY       I      Number Y pixels
C      NP       I      Number model images
C   Output:
C      MODELS   R(*)   Models (NX,NY,NP)
C      IRET     I      > 0 -> quit
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NP, IRET
      REAL      MODELS(NX,NY,*)
C
      INCLUDE 'SPFIX.INC'
      CHARACTER IFILE*48
      INTEGER   LUNO, BOI, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5,I6, I7, CORN(7), BOTEMP, IBIND, INDO, I, LIM1,
     *   WINT(4)
      LOGICAL   T, F, TBLNKD
      DOUBLE PRECISION D, DMIN, DMAX
      REAL      DBGB, DBGS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNO /18/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      TBLNKD = .FALSE.
      DMIN = 1.0D110
      DMAX = -DMIN
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
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
C                                       Setup for I/O
      WINT(1) = 1
      WINT(2) = 1
      WINT(3) = LIM1
      WINT(4) = NX
C                                       Loop
      DO 700 I7 = 1,LIM7
         CORN(7) = I7
         DO 600 I6 = 1,LIM6
            CORN(6) = I6
            DO 500 I5 = 1,LIM5
               CORN(5) = I5
               DO 400 I4 = 1,LIM4
                  CORN(4) = I4
                  DO 300 I3 = 1,LIM3
                     CORN(3) = I3
C                                       Init. files, first input.
                     CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3),
     *                  BOTEMP, IRET)
                     BOI = BOTEMP + 1
                     CALL MINIT ('WRIT', LUNO, INDO, LIM1, NX, WINT,
     *                  BUFF1, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT WRITE TO OUTPUT'
                        GO TO 990
                        END IF
                     DO 250 I2 = 1,LIM2
                        CORN(2) = I2
C                                       Read.
                        CALL MDISK ('WRIT', LUNO, INDO, BUFF1, IBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITING OUTPUT FILE'
                           GO TO 990
                           END IF
C                                       Compute
                        DO 165 I1 = 1,LIM1
                           DBGB = MODELS(I2,I3,1)
                           DBGS = MODELS(I2,I3,2)
                           IF ((MODELS(I2,I3,1).EQ.FBLANK) .OR.
     *                        (MODELS(I2,I3,2).EQ.FBLANK)) THEN
                              D = FBLANK
                              TBLNKD = .TRUE.
                           ELSE IF (MODELS(I2,I3,1).LE.0.0) THEN
                              D = FBLANK
                              TBLNKD = .TRUE.
                           ELSE
                              D = LOG10 (MODELS(I2,I3,1)) +
     *                           MODELS(I2,I3,2) * XFREQ(I1)
                              IF (NP.EQ.3) D = D + MODELS(I2,I3,3) *
     *                           XFREQ(I)**2
                              D = 10.0D0 ** D
                              DMIN = MIN (D, DMIN)
                              DMAX = MAX (D, DMAX)
                              END IF
                           BUFF1(IBIND+I1-1) = D
 165                       CONTINUE
 250                    CONTINUE
C                                       Flush buffers.
                     CALL MDISK ('FINI', LUNO, INDO, BUFF1, IBIND, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'FINISH PLANE WRITE TO OUTPUT'
                        GO TO 990
                        END IF
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       header
      CATR(KRDMN) = DMIN
      CATR(KRDMX) = DMAX
      CATR(KRBLK) = 0.0
      IF (TBLNKD) CATR(KRBLK) = FBLANK
C                                       Close files
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPFIXM: ERROR',I3,' ON ',A)
      END
      SUBROUTINE SPFIXD (NX, NY, NP, MODELS, IRET)
C-----------------------------------------------------------------------
C   SPFIXR writes the data-model cube out
C   Inputs:
C      NX       I      Number X pixels
C      NY       I      Number Y pixels
C      NP       I      Number model images
C   Output:
C      MODELS   R(*)   Models (NX,NY,NP)
C      IRET     I      > 0 -> quit
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NP, IRET
      REAL      MODELS(NX,NY,*)
C
      INCLUDE 'SPFIX.INC'
      CHARACTER IFILE*48
      INTEGER   LUNO, BOI, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5,I6, I7, CORN(7), BOTEMP, IBIND, INDO, I, LIM1,
     *   WINT(4), LUNI, INDI, WINI(4), IPOS(7), OBIND, IROUND, NXI, NYI
      LOGICAL   T, F, TBLNKD
      DOUBLE PRECISION D, DMIN, DMAX
      REAL      DBGB, DBGS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16,18/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      TBLNKD = .FALSE.
      DMIN = 1.0D110
      DMAX = -DMIN
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
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
C                                       Setup for I/O
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      WINT(1) = 1
      WINT(2) = 1
      WINT(3) = LIM1
      WINT(4) = NX
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
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
                     CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3),
     *                  BOTEMP, IRET)
                     BOI = BOTEMP + 1
                     CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI,
     *                  BUFF1, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'READ INPUT IMAGE'
                        GO TO 990
                        END IF
                     CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3),
     *                  BOTEMP, IRET)
                     BOI = BOTEMP + 1
                     CALL MINIT ('WRIT', LUNO, INDO, LIM1, NX, WINT,
     *                  BUFF2, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT WRITE TO OUTPUT'
                        GO TO 990
                        END IF
                     DO 250 I2 = 1,LIM2
                        CORN(2) = I2
C                                       Read.
                        CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'READING INPUT FILE'
                           GO TO 990
                           END IF
                        CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITING OUTPUT FILE'
                           GO TO 990
                           END IF
C                                       Compute
                        DO 165 I1 = 1,LIM1
                           DBGB = MODELS(I2,I3,1)
                           DBGS = MODELS(I2,I3,2)
                           IF ((MODELS(I2,I3,1).EQ.FBLANK) .OR.
     *                        (MODELS(I2,I3,2).EQ.FBLANK)) THEN
                              D = FBLANK
                              TBLNKD = .TRUE.
                           ELSE IF (MODELS(I2,I3,1).LE.0.0) THEN
                              D = FBLANK
                              TBLNKD = .TRUE.
                           ELSE IF (BUFF1(IBIND+I1-1).EQ.FBLANK) THEN
                              D = FBLANK
                              TBLNKD = .TRUE.
                           ELSE
                              D = LOG10 (MODELS(I2,I3,1)) +
     *                           MODELS(I2,I3,2) * XFREQ(I1)
                              IF (NP.EQ.3) D = D + MODELS(I2,I3,3) *
     *                           XFREQ(I)**2
                              D = BUFF1(IBIND+I1-1) - 10.0D0 ** D
                              DMIN = MIN (D, DMIN)
                              DMAX = MAX (D, DMAX)
                              END IF
                           BUFF2(OBIND+I1-1) = D
 165                       CONTINUE
 250                    CONTINUE
C                                       Flush buffers.
                     CALL MDISK ('FINI', LUNO, INDO, BUFF2, IBIND, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'FINISH PLANE WRITE TO OUTPUT'
                        GO TO 990
                        END IF
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       header
      CATR(KRDMN) = DMIN
      CATR(KRDMX) = DMAX
      CATR(KRBLK) = 0.0
      IF (TBLNKD) CATR(KRBLK) = FBLANK
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPFIXD: ERROR',I3,' ON ',A)
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
      INCLUDE 'SPFIX.INC'
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
      SUBROUTINE SPFIXH
C-----------------------------------------------------------------------
C   SPIXHI copies and updates history file.
C-----------------------------------------------------------------------
C
      CHARACTER HILINE*72, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR
      LOGICAL   T
      INCLUDE 'SPFIX.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKI2, DISKO, CNO2, NEWCNO, CATBLK,
     *   SCRTCH(257), SCRTCH, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 50
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCO2 (TSKNAM, NAMEI2, 'B TEMP', SEQI2, DISKI2, LUN2,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCOO (TSKNAM, NAMOUT, CLASOU, SEQOUT, DISKO, LUN2,
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
C                                       ref freq
      WRITE (HILINE,2005) TSKNAM, DEFREQ*1.D-9
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       OPCODE
      IF (OPCODE.EQ.'DIFF') THEN
         WRITE (HILINE,2010) TSKNAM, OPCODE, 'Data - model'
      ELSE
         WRITE (HILINE,2010) TSKNAM, OPCODE, 'Model (no data)'
         END IF
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       OPTYPE
      IF (OPTYPE.EQ.'CURV') THEN
         WRITE (HILINE,2015) TSKNAM, OPTYPE, 'Model includes curvature'
      ELSE
         WRITE (HILINE,2015) TSKNAM, OPTYPE, 'Model no curvature'
         END IF
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                       Copy CC files and others
      CALL ALLTAB (0, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
C                                        Update CATBLK and close
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', SCRTCH, IERR)
      FRW(1) = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPIXHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
 2005 FORMAT (A6,'REFREQ = ',F9.4,4X,'/ SPIXR reference frequency GHz')
 2010 FORMAT (A6,'OPCODE = ''',A,'''   / ',A)
 2015 FORMAT (A6,'OPTYPE = ''',A,'''   / ',A)
      END
