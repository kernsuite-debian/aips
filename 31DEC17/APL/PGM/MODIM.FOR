LOCAL INCLUDE 'MODIM.INC'
C                                       Local include for MODIM
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAXGAU
      PARAMETER (MAXGAU = 9999)
C
      HOLLERITH XNAMEI(3), XCLASI(2), XNAMEQ(3), XCLASQ(2), XNAMEU(3),
     *   XCLASU(2), XNAMOU(3), XOPCOD, XOPTYP, XINLST(12)
      REAL      XSEQI, XDISKI, XSEQQ, XDISKQ, XSEQU, XDISKU, XSEQO,
     *   XDISKO, BLC(7), TRC(7), FLUX, FACTOR, COORD(6), XIMSIZ(2),
     *   CELLS(2), APARM(10)
      REAL      BUFF(MABFSL), IONE(MAXGAU), QONE(MAXGAU), UONE(MAXGAU),
     *   SPIX(MAXGAU), RM(MAXGAU), RMTHIK(MAXGAU), FPOS(2,MAXGAU),
     *   FWID(3,MAXGAU)
      INTEGER   SEQI(3), SEQO(3), DISKI(3), DISKO(3), NPOL, NEWCNO(3),
     *   OLDCNO(3), JBUFSZ, CATOLD(256,3), CATNEW(256,3), ICODE, NGAUSS,
     *   SCRTCH(256), ICODES(MAXGAU), SPIXDO
      CHARACTER NAMEI(3)*12, CLASI(3)*6, NAMOUT*12, CLASO(3)*6,
     *   OPCODE*4, INLIST*48, OPTYPE*4
      LOGICAL   DONEW, FQAXIS
      DOUBLE PRECISION FQVAL(MAXGAU)
      COMMON /INPARM/ XNAMEI, XCLASI, XSEQI, XDISKI, XNAMEQ, XCLASQ,
     *   XSEQQ, XDISKQ, XNAMEU, XCLASU, XSEQU, XDISKU, XNAMOU, XSEQO,
     *   XDISKO, BLC, TRC, FLUX, FACTOR, XOPCOD, XOPTYP, XINLST, COORD,
     *   XIMSIZ, CELLS, APARM
      COMMON /CHRCOM/ NAMEI, CLASI, NAMOUT, CLASO, OPCODE, OPTYPE,
     *   INLIST
      COMMON /PARMS/ FQVAL, CATOLD, CATNEW, SEQI, SEQO, DISKI, DISKO,
     *   NPOL,NEWCNO, OLDCNO, JBUFSZ, ICODE, DONEW, NGAUSS, SPIXDO,
     *   FQAXIS
      COMMON /BUFRS/ IONE, QONE, UONE, SPIX, RM, RMTHIK, FPOS, FWID,
     *   ICODES, BUFF, SCRTCH
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       End MODIM
LOCAL END
      PROGRAM MODIM
C-----------------------------------------------------------------------
C! Adds a model to a pair of new or old Q and U image cubes
C# Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2012-2015
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
C   MODIM is an AIPS task to modify an image by a model
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEI         Name of Q input image.
C      INCLASS        CLASI         Class of Q input image.
C      INSEQ          SEQI          Seq. of Q input image.
C      INDISK         DISKI         Disk number of Q input image.
C      IN2NAME        NAMEI         Name of U input image.
C      IN2CLASS       CLASI         Class of U input image.
C      IN2SEQ         SEQI          Seq. of U input image.
C      IN2DISK        DISKI         Disk number of U input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLASO         Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQO          Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      OPCODE         OPCODE        User specified opcode.
C      FLUX           FLUX          Noise level in Jy/Pix.
C      FACTOR         FACTOR        Multiplying factor for previous
C                                   data.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NX, NY, NEED
      LONGINT   PIMAGE
      REAL      IMAGE(2)
      INCLUDE 'MODIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'MODIM '/
C-----------------------------------------------------------------------
C                                       Get inputs, create output file
      CALL MODIMI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get memory for 1 plane
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
      NEED = (NX * NY - 1) / 1024 + 2
      CALL ZMEMRY ('GET ', TSKNAM, NEED, IMAGE, PIMAGE, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'FAILED TO GET NEEDED DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Apply model to image
      CALL MODIMD (NX, NY, IMAGE(1+PIMAGE), IRET)
C                                       Add history
      IF (IRET.EQ.0) CALL MODIMH
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE MODIMI (PRGN, IRET)
C-----------------------------------------------------------------------
C   MODIMI gets input parameters for MODIM and creates an output file.
C   Inputs:
C      PRGN    C*6       Program name
C   Output:
C      IRET    I         Error code: 0 => ok
C                           4 => user routine detected error.
C                           5 => catalog troubles
C                           8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER STAT*4, MTYPE*2, CODES(6)*4
      INTEGER   IERR, NPARM, IROUND, NCODE, I, FQAX
      INCLUDE 'MODIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA NCODE /6/
      DATA CODES /'POIN','GAUS','DISK','RECT','SPHE','EXPD'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSL
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 76
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
      NPOL = 1
C                                       Crunch input parameters.
      SEQI(1) = IROUND (XSEQI)
      SEQI(2) = IROUND (XSEQQ)
      SEQI(3) = IROUND (XSEQU)
      SEQO(1) = IROUND (XSEQO)
      SEQO(2) = IROUND (XSEQO)
      SEQO(3) = IROUND (XSEQO)
      DISKI(1) = IROUND (XDISKI)
      DISKI(2) = IROUND (XDISKQ)
      DISKI(3) = IROUND (XDISKU)
      DISKO(1) = IROUND (XDISKO)
      DISKO(2) = IROUND (XDISKO)
      DISKO(3) = IROUND (XDISKO)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEI(1))
      CALL H2CHR (6, 1, XCLASI, CLASI(1))
      CALL H2CHR (12, 1, XNAMEQ, NAMEI(2))
      CALL H2CHR (6, 1, XCLASQ, CLASI(2))
      CALL H2CHR (12, 1, XNAMEU, NAMEI(3))
      CALL H2CHR (6, 1, XCLASU, CLASI(3))
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CLASO(1) = 'IMODEL'
      CLASO(2) = 'QMODEL'
      CLASO(3) = 'UMODEL'
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (48, 1, XINLST, INLIST)
      IF (INLIST.EQ.' ') THEN
         MSGTXT = 'AN INLIST MUST BE SPECIFIED'
         GO TO 990
         END IF
C                                       Set default OPCODE
      ICODE = 2
      DO 10 I = 1,NCODE
         IF (OPCODE.EQ.CODES(I)) ICODE = I
 10      CONTINUE
C                                       Default to point model if nec.
      OPCODE = CODES(ICODE)
      MSGTXT = 'Using default OPCODE = ''' // OPCODE // ''''
      CALL MSGWRT (3)
C                                       get components, err msg if error
      SPIXDO = 0
      CALL READIT (IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SPIXDO.EQ.2) THEN
         IF (OPTYPE.EQ.'GAUS') SPIXDO = 3
         IF (OPTYPE.EQ.'EXP ') SPIXDO = 4
         END IF
      WRITE (MSGTXT,1005) NGAUSS
      CALL MSGWRT (3)
      IF (NGAUSS.LE.0) THEN
         IRET = 10
         GO TO 999
         END IF
      IF (NPOL.EQ.1) THEN
         DONEW = (NAMEI(1).EQ.' ')
      ELSE
         DONEW = (NAMEI(1).EQ.' ') .OR. (NAMEI(2).EQ.' ') .OR.
     *      (NAMEI(3).EQ.' ')
         END IF
C                                       Get CATBLK from old file.
      MTYPE = 'MA'
      IF (.NOT.DONEW) THEN
         DO 20 I = 1,NPOL
            OLDCNO(I) = 1
            CALL CATDIR ('SRCH', DISKI(I), OLDCNO(I), NAMEI(I),
     *         CLASI(I), SEQI(I), MTYPE, NLUSER, STAT, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR, NAMEI(I), CLASI(I), SEQI(I),
     *            DISKI(I), NLUSER
               GO TO 990
               END IF
C                                       Read CATBLK and mark 'READ'.
            CALL CATIO ('READ', DISKI(I), OLDCNO(I), CATOLD(1,I),
     *         'READ', SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR, I
               GO TO 990
               END IF
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKI(I)
            FCNO(NCFILE) = OLDCNO(I)
            FRW(NCFILE) = 0
            CALL AXEFND (4, 'FQID', CATOLD(KIDIM,I), CATOLD(KHCTP,I),
     *         FQAX, IERR)
            IF (I.EQ.1) THEN
               FQAXIS = (IERR.EQ.0) .AND. (FQAX.GE.0)
               IF (FQAXIS) THEN
                  CALL FNDFQ (I, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1015) IERR, I
                     GO TO 990
                     END IF
                  END IF
            ELSE IF (FQAXIS) THEN
               IF ((IERR.NE.0) .OR. (FQAX.LT.0)) THEN
                  MSGTXT = 'Q OR U IMAGE HAS NO FQID AXIS'
                  IERR = 10
                  GO TO 990
               ELSE
                  CALL FNDFQ (I, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1015) IERR, I
                     GO TO 990
                     END IF
                  END IF
               END IF
 20         CONTINUE
C                                       Make a new header
      ELSE
         CALL RFILL (7, 0.0, BLC)
         CALL RFILL (7, 0.0, TRC)
         CALL NEWHDR
         FQAXIS = .FALSE.
         END IF
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM,1), CATOLD(KINAX,1), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       create outputs
      DO 40 I = 1,NPOL
C                                       Copy old CATBLK to new.
         CALL COPY (256, CATOLD(1,I), CATBLK)
C                                       Put new values in CATBLK.
         CALL MAKOUT (NAMEI(I), CLASI(I), SEQI(I), '      ', NAMOUT,
     *      CLASO(I), SEQO(I))
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLASO(I), KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQO(I)
C                                       Get user modification to CATBLK
         IF (.NOT.DONEW) CALL IMMHED
C                                       Create output file.
         NEWCNO(I) = 1
         IRET = 4
         CALL MCREAT (DISKO(I), NEWCNO(I), SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR, I
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO(I)
         FCNO(NCFILE) = NEWCNO(I)
         FRW(NCFILE) = 2
         SEQO(I) = CATBLK(KIIMS)
C                                       keywords copied mostly
         IF (.NOT.DONEW) CALL KEYPCP (DISKI(I), OLDCNO(I), DISKO(I),
     *      NEWCNO(I), 0, ' ', IERR)
         CALL COPY (256, CATBLK, CATNEW(1,I))
 40      CONTINUE
C                                       init random number generator
      IF (FLUX.GT.0.0) CALL RANDIN (I)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODIMI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1005 FORMAT ('Will use',I5,' model components')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1015 FORMAT ('ERROR',I4,' READING FQ TABLE FOR IMAGE',I2)
 1020 FORMAT ('ERROR',I3,' COPYING CATBLK IMAGE',I2)
 1030 FORMAT ('MODIMI: ERROR',I3,' CREATING OUTPUT FILE',I2)
      END
      SUBROUTINE FNDFQ (IMAGE, IERR)
C-----------------------------------------------------------------------
C   FNDFQ reads the values of the FQ table
C   Inputs:
C      IMAGE   I   Which image: 1 get values, 2 and 3 check them
C   Outputs:
C      IERR    I   0 okay, 10 not match, else I/O error
C-----------------------------------------------------------------------
      INTEGER   IMAGE, IERR
C
      INCLUDE 'MODIM.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, J, FQAX, FRAX, FQBUF(512), FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC), IFQRNO, IFQ, FQID, IFSIDE, FQVER, NUMIF,
     *   NUMREC, LUN, LUNTMP, IQFREQ
      REAL      IFCHW, IFTBW
      DOUBLE PRECISION IFFREQ, FREQV
      CHARACTER BNDCOD*8
C-----------------------------------------------------------------------
      CALL COPY (256, CATOLD(1,IMAGE), CATBLK)
      J = IMAGE
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), FRAX, IERR)
      IF ((IERR.NE.0) .OR. (FRAX.LT.0)) THEN
         WRITE (MSGTXT,1000) 'NO FREQ AXIS', J
         IERR = 10
         GO TO 990
         END IF
      CALL AXEFND (4, 'FQID', CATBLK(KIDIM), CATH(KHCTP), FQAX, IERR)
      IF ((IERR.NE.0) .OR. (FRAX.LT.0)) THEN
         WRITE (MSGTXT,1000) 'NO FQID AXIS', J
         IERR = 10
         GO TO 990
         END IF
      LUN = LUNTMP (1)
      FQVER = 1
      CALL FQINI ('READ', FQBUF, DISKI(J), OLDCNO(J), FQVER, CATBLK,
     *   LUN, IFQRNO, FQKOLS, FQNUMV, NUMIF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'OPENING FQ FILE', J
         GO TO 990
         END IF
      NUMREC = FQBUF(5)
      DO 20 IFQRNO = 1,NUMREC
         IFQ = IFQRNO
         CALL TABFQ ('READ', FQBUF, IFQ, FQKOLS, FQNUMV, NUMIF, FQID,
     *      IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'READING FQ TABLE', J
            GO TO 990
            END IF
         IQFREQ = (FQID - CATD(KDCRV+FQAX)) / CATR(KRCIC+FQAX) +
     *      CATR(KRCRP+FQAX)
         I = IQFREQ + 0.1D0
         IF ((I.GE.1) .AND. (I.LE.CATBLK(KIDIM+FQAX))) THEN
            FREQV = IFFREQ + CATD(KDCRV+FRAX)
            IF (J.EQ.1) THEN
               FQVAL(I) = FREQV
            ELSE
               IF (ABS(FQVAL(I)-FREQV).GT.0.01*IFCHW) THEN
                  IERR = 10
                  WRITE (MSGTXT,1000) 'FQ VALUES DO NOT MATCH', J
                  GO TO 990
                  END IF
               END IF
            END IF
 20      CONTINUE
      CALL TABFQ ('CLOS', FQBUF, IFQ, FQKOLS, FQNUMV, NUMIF, FQID,
     *   IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDFQ: ERROR ',A,' IMAGE',I2)
 1010 FORMAT ('FNDFQ: ERROR',I3,' ON ',A,' IMAGE',I2)
      END
      SUBROUTINE NEWHDR
C-----------------------------------------------------------------------
C   NEWHDR makes up image headers from scratch
C-----------------------------------------------------------------------
C
      INTEGER   DATE(3), I
      CHARACTER STRNG*8
      INCLUDE 'MODIM.INC'
C-----------------------------------------------------------------------
C                                       blank slate
      CALL CATINI (CATBLK)
      CALL CHR2H (8, 'IQUmodel', 1, CATH(KHOBJ))
      CALL CHR2H (8, 'JY/BEAM ', 1, CATH(KHBUN))
      CALL ZDATE (DATE)
      WRITE (STRNG,1000) DATE
      CALL CHR2H (8, STRNG, 1, CATH(KHDMP))
      CALL CHR2H (8, STRNG, 1, CATH(KHDOB))
      CATR(KREPO) = 2000.0
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIDIM) = 4
C                                       RA
      I = XIMSIZ(1) + 0.5
      IF (I.LE.0) I = 512
      CATBLK(KINAX) = I
      CATD(KDCRV) = (COORD(1)*15.D0 + COORD(2)/4.D0 + COORD(3)/240.D0)
      CATR(KRCRP) = (I + 1) / 2
      IF (CELLS(1).EQ.0.0) CELLS(1) = 1.
      CATR(KRCIC) = -ABS(CELLS(1)) / 3600.0
      CALL CHR2H (8, 'RA---SIN', 1, CATH(KHCTP))
C                                       DEC
      I = XIMSIZ(2) + 0.5
      IF (I.LE.0) I = 512
      CATBLK(KINAX+1) = I
      CATD(KDCRV+1) = (COORD(4) + COORD(5)/60.D0 + COORD(6)/3600.D0)
      CATR(KRCRP+1) = (I + 2) / 2
      IF (CELLS(1).EQ.0.0) CELLS(1) = 1.
      CATR(KRCIC+1) = ABS(CELLS(1)) / 3600.0
      CALL CHR2H (8, 'DEC--SIN', 1, CATH(KHCTP+2))
C                                       FREQ
      I = APARM(3) + 0.5
      IF (I.LE.0) I = 512
      CATBLK(KINAX+2) = I
      CATD(KDCRV+2) = APARM(1) * 1.D9
      CATR(KRCRP+2) = 1.0
      IF (APARM(2).EQ.0.0) APARM(2) = 0.001
      CATR(KRCIC+2) = APARM(2) * 1.E9
      CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP+4))
C                                       STOKES
      CATBLK(KINAX+3) = 1
      CATD(KDCRV+3) = 1.0D0
      CATR(KRCRP+3) = 1.0
      CATR(KRCIC+3) = 1.0
      CALL CHR2H (8, 'STOKES  ', 1, CATH(KHCTP+6))
C                                       clean beam fake
      CATR(KRBMJ) = 3.0 * ABS (CATR(KRCIC))
      CATR(KRBMN) = 3.0 * ABS (CATR(KRCIC))
C                                       to output
      CALL COPY (256, CATBLK, CATOLD(1,1))
      CATD(KDCRV+3) = 2.0D0
      CALL COPY (256, CATBLK, CATOLD(1,2))
      CATD(KDCRV+3) = 3.0D0
      CALL COPY (256, CATBLK, CATOLD(1,3))
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4,2I2.2)
      END
      SUBROUTINE IMMHED
C-----------------------------------------------------------------------
C   Adjust the output header for blc, trc
C-----------------------------------------------------------------------
C
      CHARACTER FCHARS(3)*4, CHTMP*4
      INTEGER   LIMIT, I, INDEX
      INCLUDE 'MODIM.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Set axes in output CATBLK.
      LIMIT = CATBLK(KIDIM)
C                                       Copy/update axis values
      DO 80 I = 1,LIMIT
         CATBLK(KINAX+I-1) = TRC(I) - BLC(I) + 1.01
         CATR(KRCRP+I-1) = CATR(KRCRP+I-1) - BLC(I) + 1.0
         IF (CATBLK(KIALT).NE.0) THEN
            INDEX = KHCTP + (I-1) * 2
            CALL H2CHR (4, 1, CATH(INDEX), CHTMP)
            IF ((CHTMP.EQ.FCHARS(1)) .OR. (CHTMP.EQ.FCHARS(2)) .OR.
     *         (CHTMP.EQ.FCHARS(3))) CATR(KRARP) = CATR(KRARP) -
     *         BLC(I) + 1.0
            END IF
 80      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE READIT (IRET)
C-----------------------------------------------------------------------
C   Prepares list of components for adverbs or text file
C   Output
C      IRET   I   Error code
C   rest in Common
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'MODIM.INC'
      INTEGER   TLUN, TIND, LUNTMP, LLIM, LP, I, JTRIM, J
      REAL      BMAJ, BMIN, BPA, XINC
      CHARACTER LINE*132
      DOUBLE PRECISION X
C-----------------------------------------------------------------------
C                                       width defaults
      XINC = ABS (CATR(KRCIC))
      BMAJ = 0.0
      IF (XINC.GT.0.0) THEN
         BMAJ = CATR(KRBMJ) / XINC
         BMIN = CATR(KRBMN) / XINC
         BPA = CATR(KRBPA)
         END IF
      IF ((BMAJ.LE.0.0) .OR. (BMIN.LE.0.0)) THEN
         BMAJ = 3.0
         BMIN = 3.0
         BPA = 0.0
         END IF
C                                       read text file
      TLUN = LUNTMP (2)
C                                       open the text file
      CALL ZTXOPN ('READ', TLUN, TIND, INLIST, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN TEXT FILE'
         GO TO 999
         END IF
      NGAUSS = 0
 100  CALL ZTXIO ('READ', TLUN, TIND, LINE, IRET)
      IF ((IRET.EQ.0) .AND. (NGAUSS.LT.MAXGAU)) THEN
         LLIM = JTRIM (LINE)
C                                       blanks, comments
         IF (LLIM.LE.0) GO TO 100
         IF (LINE(1:1).EQ.'#') GO TO 100
C                                       parse
C                                       I flux
         LP = 1
         CALL GETNUM (LINE, LLIM, LP, X)
         IF ((X.EQ.DBLANK) .OR. (X.EQ.0.0D0)) THEN
            GO TO 100
         ELSE
            NGAUSS = NGAUSS + 1
            IONE(NGAUSS) = X
            END IF
C                                       Q flux
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            QONE(NGAUSS) = X
            IF (X.NE.0.0D0) NPOL = 3
            END IF
C                                       U flux
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            UONE(NGAUSS) = X
            IF (X.NE.0.0D0) NPOL = 3
            END IF
C                                       Spectral index
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            SPIX(NGAUSS) = X
            END IF
C                                       RM
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            RM(NGAUSS) = X
            END IF
C                                       RM thickness
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            RMTHIK(NGAUSS) = X
            IF (X.GT.0.0D0) SPIXDO = 2
            END IF
C                                       position
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            FPOS(1,NGAUSS) = X
            END IF
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            FPOS(2,NGAUSS) = X
            END IF
C                                       width
         CALL RFILL (3, 0.0, FWID(1,NGAUSS))
         DO 110 J = 1,3
            IF (LP.LE.LLIM) THEN
               CALL GETNUM (LINE, LLIM, LP, X)
               IF (X.EQ.DBLANK) THEN
                  NGAUSS = NGAUSS - 1
                  GO TO 100
               ELSE
                  FWID(J,NGAUSS) = X
                  END IF
               END IF
 110        CONTINUE
C                                       model type
         ICODES(NGAUSS) = 0
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) THEN
               NGAUSS = NGAUSS - 1
               GO TO 100
            ELSE
               ICODES(NGAUSS) = X + 0.01
               END IF
            END IF
         IF ((ICODES(NGAUSS).LT.1) .OR. (ICODES(NGAUSS).GT.6))
     *      ICODES(NGAUSS) = ICODE
         GO TO 100
C                                       real error
      ELSE IF ((IRET.GT.0) .AND. (IRET.NE.2)) THEN
         WRITE (MSGTXT,1000) IRET, 'READING TEXT FILE'
         GO TO 999
C                                       EOF
      ELSE
         CALL ZTXCLS (TLUN, TIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSING TEXT FILE'
            GO TO 999
            END IF
         END IF
C                                       check defaults
      DO 120 I = 1,NGAUSS
         IF ((FWID(1,I).LE.0.0) .OR. (FWID(2,I).LE.0.0)) THEN
            FWID(1,I) = BMAJ
            FWID(2,I) = BMIN
            FWID(3,I) = BPA
            END IF
 120     CONTINUE
C                                       MSGWRT left to caller
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('READIT ERROR',I4,' ON ',A)
      END
      SUBROUTINE MODIMD (NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   MODIMD read in the input images one plane at a time, adds the model
C   appropriate to that plane, and then writes out the plane
C   Input:
C      NX      I      Number X pixels
C      NY      I      Nu,ber Y pixels
C   Output:
C      IMAGE   R(*)   Adequate memory for a plane
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      IMAGE(NX,*)
C
      CHARACTER IFILE*48, POLS(3)*1
      INTEGER   IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO,
     *   WINO(4), BOI, BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I3, I4,
     *   I5, I6, I7, IPOS(7), CORN(7), BOTEMP, LIMO, IBIND, OBIND, INDI,
     *   INDO, LIM1, FRAX, J, IX, IY
      REAL      OUTMAX, OUTMIN
      DOUBLE PRECISION LAMBSQ, FREQ
      LOGICAL   T, F, BLNKD
      INCLUDE 'MODIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
      DATA POLS /'I','Q','U'/
C-----------------------------------------------------------------------
C                                       loop over polarization
      DO 100 J = 1,NPOL
         MSGTXT = 'Beginning ' // POLS(J) // ' polarization processing'
         CALL MSGWRT (2)
         CALL COPY (256, CATNEW(1,J), CATBLK)
         IF (FQAXIS) THEN
            CALL AXEFND (4, 'FQID', CATBLK(KIDIM), CATH(KHCTP), FRAX,
     *         IRET)
         ELSE
            CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), FRAX,
     *         IRET)
            END IF
         IF (IRET.NE.0) THEN
            MSGTXT = 'CANNOT FIND FREQ/FQID AXIS'
            GO TO 990
            END IF
C                                       Open and init for read
         IF (.NOT.DONEW) THEN
            CALL ZPHFIL ('MA', DISKI(J), OLDCNO(J), 1, IFILE, IRET)
            CALL ZOPEN (LUNI, INDI, DISKI(J), IFILE, T, F, T, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPEN INPUT', J
               GO TO 990
               END IF
            END IF
         CALL ZPHFIL ('MA', DISKO(J), NEWCNO(J), 1, IFILE, IRET)
         CALL ZOPEN (LUNO, INDO, DISKO(J), IFILE, T, T, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUPUT', J
            GO TO 990
            END IF
C                                       Setup for I/O
         NXI = CATOLD(KINAX,J)
         NYI = CATOLD(KINAX+1,J)
         NXO = CATBLK(KINAX)
         NYO = CATBLK(KINAX+1)
         WINI(1) = IROUND (BLC(1))
         WINI(2) = IROUND (BLC(2))
         WINI(3) = IROUND (TRC(1))
         WINI(4) = IROUND (TRC(2))
         WINO(1) = 1
         WINO(2) = 1
         WINO(3) = NXO
         WINO(4) = NYO
         OUTMAX = -1.0E30
         OUTMIN = 1.0E30
         BLNKD = F
C                                       Setup for looping
         LIM1 = TRC(1) - BLC(1) + 1.01
         LIM2 = TRC(2) - BLC(2) + 1.01
         LIM3 = TRC(3) - BLC(3) + 1.01
         LIM4 = TRC(4) - BLC(4) + 1.01
         LIM5 = TRC(5) - BLC(5) + 1.01
         LIM6 = TRC(6) - BLC(6) + 1.01
         LIM7 = TRC(7) - BLC(7) + 1.01
         CORN(7) = 1
         LIMO = CATBLK(KINAX) - 1
C                                       Loop
         IPOS(1) = WINI(1)
         DO 90 I7 = 1,LIM7
            IPOS(7) = BLC(7) + I7 - 0.9
            CORN(7) = I7
            DO 85 I6 = 1,LIM6
               IPOS(6) = BLC(6) + I6 - 0.9
               CORN(6) = I6
               DO 80 I5 = 1,LIM5
                  IPOS(5) = BLC(5) + I5 - 0.9
                  CORN(5) = I5
                  DO 75 I4 = 1,LIM4
                     IPOS(4) = BLC(4) + I4 - 0.9
                     CORN(4) = I4
                     DO 70 I3 = 1,LIM3
                        IPOS(3) = BLC(3) + I3 - 0.9
                        CORN(3) = I3
C                                       Init. files, first input.
                        IF (DONEW) THEN
                           IY = NX * NY
                           CALL RFILL (IY, 0.0, IMAGE)
                        ELSE
                           CALL COMOFF (CATOLD(KIDIM,J),
     *                        CATOLD(KINAX,J), IPOS(3), BOTEMP,IRET)
                           BOI = BOTEMP + 1
                           CALL MINIT ('READ', LUNI, INDI, NXI, NYI,
     *                        WINI, BUFF, JBUFSZ, BOI, IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET, 'INIT INPUT', J
                              GO TO 990
                              END IF
                           DO 20 IY = 1,LIM2
                              IPOS(2) = BLC(2) + IY - 0.99
C                                       Read.
                              CALL MDISK ('READ', LUNI, INDI, BUFF,
     *                           IBIND, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET, 'READ INPUT',
     *                              J
                                 GO TO 990
                                 END IF
                              CALL RCOPY (NXO, BUFF(IBIND), IMAGE(1,IY))
 20                           CONTINUE
                           END IF
C                                       compute the model even lower
                        IF (FQAXIS) THEN
                           FREQ = FQVAL(IPOS(FRAX+1))
                        ELSE
                           FREQ = CATD(KDCRV+FRAX) + CATR(KRCIC+FRAX) *
     *                        (IPOS(FRAX+1) - CATR(KRCRP+FRAX))
                           END IF
                        LAMBSQ = (VELITE / FREQ) ** 2
                        CALL THEMOD (J, FREQ, LAMBSQ, NX, NY, IMAGE)
                        DO 30 IY = 1,NYO
                           DO 25 IX = 1,NXO
                              IF (IMAGE(IX,IY).EQ.FBLANK) THEN
                                 BLNKD  = .TRUE.
                              ELSE
                                 OUTMAX = MAX (OUTMAX, IMAGE(IX,IY))
                                 OUTMIN = MIN (OUTMIN, IMAGE(IX,IY))
                                 END IF
 25                           CONTINUE
 30                        CONTINUE
C                                       Init output file.
                        CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX),
     *                     CORN(3), BOTEMP, IRET)
                        BOO = BOTEMP + 1
                        CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO,
     *                     BUFF, JBUFSZ, BOO, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT', J
                           GO TO 990
                           END IF
                        DO 40 IY = 1,LIM2
                           IPOS(2) = BLC(2) + IY - 0.99
C                                       Write.
                           CALL MDISK ('WRIT', LUNO, INDO, BUFF, OBIND,
     *                        IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT',
     *                           J
                              GO TO 990
                              END IF
                           CALL RCOPY (NX, IMAGE(1,IY), BUFF(OBIND))
 40                        CONTINUE
C                                       Flush buffer.
                        CALL MDISK ('FINI', LUNO, INDO, BUFF, OBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'FINISH OUTPUT', J
                           GO TO 990
                           END IF
 70                     CONTINUE
 75                  CONTINUE
 80               CONTINUE
 85            CONTINUE
 90         CONTINUE
C                                       Mark blanking in CATBLK.
         CATR(KRBLK) = 0.0
         IF (BLNKD) CATR(KRBLK) = FBLANK
         CATR(KRDMX) = OUTMAX
         CATR(KRDMN) = OUTMIN
         CALL COPY (256, CATBLK, CATNEW(1,J))
         CALL CATIO ('UPDT', DISKO(J), NEWCNO(J), CATBLK, 'REST',
     *      SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'UPDATING HEADER OF', J
            GO TO 990
            END IF
C                                       Close images
         IF (.NOT.DONEW) CALL ZCLOSE (LUNI, INDI, IRET)
         CALL ZCLOSE (LUNO, INDO, IRET)
 100     CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODIMD: ERROR',I3,' ON ',A,' FILE',I2)
      END
      SUBROUTINE THEMOD (J, FREQ, LAMBSQ, NX, NY, IMAGE)
C-----------------------------------------------------------------------
C   Apply model to one plane of the image
C   Inputs:
C      J        I      J = 1 -> I, J = 2 -> Q, J = 3 -> U
C      FREQ     D      Frequency in Hz
C      LAMBSQ   D      Lambda squared im m^2
C      NX       I      Number X pixels
C      NY       I      Number Y pixels
C   In/out
C      IMAGE    R(NX,*)   image
C-----------------------------------------------------------------------
      INTEGER   J, NX, NY
      REAL      IMAGE(NX,*)
      DOUBLE PRECISION FREQ, LAMBSQ
C
      INCLUDE 'MODIM.INC'
      REAL      XX, YY, CPHI(MAXGAU), SPHI(MAXGAU), R, ANOISE, FMAX
      INTEGER   IX, IY, II, JJ, K, ISET, IXBLC, IYBLC, I
      REAL      MV, RSUM
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE ISET, IYBLC, IXBLC, CPHI, SPHI
      DATA ISET /0/
C-----------------------------------------------------------------------
C                                       Initialize constants
      IF (ISET.EQ.0) THEN
         ISET = 1
         DO 10 I = 1,NGAUSS
            CPHI(I) = COS (FWID(3,I) * DG2RAD)
            SPHI(I) = SIN (FWID(3,I) * DG2RAD)
 10         CONTINUE
C                                       Convert x-window to integers
         IXBLC = BLC(1) + 0.01
         IYBLC = BLC(2) + 0.01
         END IF
C                                       Loop over image and apply model
C                                       Point
      DO 130 IY = 1,NY
         JJ = IY + IYBLC - 1
         DO 120 IX = 1,NX
            II = IX + IXBLC - 1
            RSUM = 0.0
            DO 110 K = 1,NGAUSS
               IF (ICODES(K).EQ.1) THEN
                  R = (II-FPOS(1,K))**2 + (JJ-FPOS(2,K))**2
                  IF (R.LE.0.25) THEN
                     CALL ROTMES (J, IONE(K), QONE(K), UONE(K), RM(K),
     *                  RMTHIK(K), SPIX(K), FREQ, LAMBSQ, SPIXDO, FMAX)
                     RSUM = RSUM + FMAX
                     END IF
               ELSE IF (ICODES(K).EQ.2) THEN
                  CALL RADPOS (II, JJ, FPOS(1,K), FWID(1,K), CPHI(K),
     *               SPHI(K), XX, YY, R)
                  IF (R.LT.2.0) THEN
                     MV =  EXP (-2.772588722 * R * R)
                     CALL ROTMES (J, IONE(K), QONE(K), UONE(K), RM(K),
     *                  RMTHIK(K), SPIX(K), FREQ, LAMBSQ, SPIXDO, FMAX)
                     RSUM = RSUM + MV * FMAX
                     END IF
               ELSE IF (ICODES(K).EQ.3) THEN
                  CALL RADPOS (II, JJ, FPOS(1,K), FWID(1,K), CPHI(K),
     *               SPHI(K), XX, YY, R)
                  IF (R.LE.0.5) THEN
                     CALL ROTMES (J, IONE(K), QONE(K), UONE(K), RM(K),
     *                  RMTHIK(K), SPIX(K), FREQ, LAMBSQ, SPIXDO, FMAX)
                     RSUM = RSUM + FMAX
                     END IF
               ELSE IF (ICODES(K).EQ.4) THEN
                  CALL RADPOS (II, JJ, FPOS(1,K), FWID(1,K), CPHI(K),
     *               SPHI(K), XX, YY, R)
                  IF ((ABS(XX).LE.0.5) .AND. (ABS(YY).LE.0.5)) THEN
                     CALL ROTMES (J, IONE(K), QONE(K), UONE(K), RM(K),
     *                  RMTHIK(K), SPIX(K), FREQ, LAMBSQ, SPIXDO, FMAX)
                     RSUM = RSUM + FMAX
                     END IF
               ELSE IF (ICODES(K).EQ.5) THEN
                  CALL RADPOS (II, JJ, FPOS(1,K), FWID(1,K), CPHI(K),
     *               SPHI(K), XX, YY, R)
                  IF (R.LT.0.5) THEN
                     MV = SQRT (1.0 - 4.0 * R * R)
                     CALL ROTMES (J, IONE(K), QONE(K), UONE(K), RM(K),
     *                  RMTHIK(K), SPIX(K), FREQ, LAMBSQ, SPIXDO, FMAX)
                     RSUM = RSUM + MV * FMAX
                     END IF
               ELSE IF (ICODES(K).EQ.6) THEN
                  CALL RADPOS (II, JJ, FPOS(1,K), FWID(1,K), CPHI(K),
     *               SPHI(K), XX, YY, R)
                  IF (R.LT.8.0) THEN
                     MV = EXP (-1.386294361 * R)
                     CALL ROTMES (J, IONE(K), QONE(K), UONE(K), RM(K),
     *                  RMTHIK(K), SPIX(K), FREQ, LAMBSQ, SPIXDO, FMAX)
                     RSUM = RSUM + MV * FMAX
                     END IF
                  END IF
 110           CONTINUE
C
            IMAGE(IX,IY) = FACTOR * IMAGE(IX,IY) + RSUM
C                                       Add random noise?
            IF (FLUX.GT.0.0) THEN
               CALL NOISE (ANOISE)
               IMAGE(IX,IY) = IMAGE(IX,IY) + ANOISE * FLUX
               END IF
 120        CONTINUE
 130     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RADPOS (I, J, FP, FW, CPHI, SPHI, XX, YY, R)
C-----------------------------------------------------------------------
C   Work out distance of current pixel from model center and normalize
C   by the FWHM and correct for p.a. of model
C   Inputs:
C      I      I      X pixel
C      J      I      Y pixel
C      FP     R(2)   Component X,Y center pixels
C      FW     R(3)   Component Bmaj, Bmin, Bpa (pixels, pixel, deg)
C      CPHI   R      Cos (Bpa)
C      SPHI   R      Sin (Bpa)
C   Outputs:
C      XX     R      Normalized X position
C      YY     R      Normalized Y position
C      R      R      Normalized radius of current pixel from model
C                     center
C   Disks and rectangles extend only to R=0.5 or XX,YY=0.5
C-----------------------------------------------------------------------
      INTEGER   I, J
      REAL      FP(2), FW(3), CPHI, SPHI, XX, YY, R
C
      REAL      X, Y
C-----------------------------------------------------------------------
      X = I - FP(1)
      Y = J - FP(2)
      XX = (Y * CPHI - X * SPHI) / FW(1)
      YY = (X * CPHI + Y * SPHI) / FW(2)
      R = SQRT (XX**2 + YY**2)
C
      RETURN
      END
      SUBROUTINE NOISE (A)
C-----------------------------------------------------------------------
C   NOISE generates a random number approximately distributed in a
C   Gaussian manner about zero.  It does it by summing a uniformly-
C   distributed random number 12 times.
C   Output:
C      A   R       The current sample from the gaussian distribution
C-----------------------------------------------------------------------
      REAL      A, B
      INTEGER   J
C-----------------------------------------------------------------------
      A = -6.0
      DO 10 J = 1,12
         CALL RANDUM (B)
         A = A + B
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ROTMES (J, IONE, QONE, UONE, RM, RMTHIK, SPIX, FREQ,
     *   LAMBSQ, SPIXDO, FMAX)
C-----------------------------------------------------------------------
C   ROTMES computes the rotation measure rotated Q or U
C   Inputs:
C      J        I   1 want I, 2 want Q, 3 want U
C      IONE     R   I at FREQ=1.E9
C      QONE     R   Q at FREQ=1.E9
C      UONE     R   U at FREQ=1.E9
C      RM       R   Rotation measure in radians/m^2
C      SPIX     R   Spectral index
C      LAMBSQ   D   Lambda squared in m^2
C      SPIXDO   I   Type of thickness model (2, 3, 4)
C   Outputs
C      FMAX     R   The Q or U at this lambda
C-----------------------------------------------------------------------
      INTEGER   J, SPIXDO
      REAL      IONE, QONE, UONE, RM, RMTHIK, SPIX, FMAX
      DOUBLE PRECISION FREQ, LAMBSQ
C
      REAL      SPHI, CPHI, FACTOR
      DOUBLE PRECISION LAMONE, XX, DSINCS
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      FACTOR = (FREQ/1.D9) ** SPIX
      IF (RMTHIK.NE.0.0) THEN
         XX = RMTHIK * LAMBSQ
         FACTOR = FACTOR * DSINCS (SPIXDO, 0, XX)
         END IF
      IF (J.GT.1) THEN
         LAMONE = (VELITE / 1.D9) ** 2
         SPHI = FACTOR * SIN (2.0 * RM * (LAMBSQ - LAMONE))
         CPHI = FACTOR * COS (2.0 * RM * (LAMBSQ - LAMONE))
         END IF
      IF (J.EQ.1) THEN
         FMAX = FACTOR * IONE
      ELSE IF (J.EQ.2) THEN
         FMAX = QONE * CPHI - UONE * SPHI
      ELSE
         FMAX = UONE * CPHI + QONE * SPHI
         END IF
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION DSINCS (MODEL, TYPE, XX)
C-----------------------------------------------------------------------
C   returns sin(xx)/xx on type 0 and d (sin(xx)/xx) / dx on type 1
C   Inputs:
C      MODEL   I   2 - sin(x)/x, 3 Gauss, 4 exp
C      TYPE    I   0 - return function, 1 return derivative
C      XX      D   argument
C   Output
C     DSINCS   D   function value or derivative
C-----------------------------------------------------------------------
      INTEGER   MODEL, TYPE
      DOUBLE PRECISION XX
C
      DOUBLE PRECISION F
C                                       1.8954 radians is the half-power
C                                       of sin(x) / x
C-----------------------------------------------------------------------
      DSINCS = 0.0D0
C                                       SIN(X)/X function
      IF (MODEL.EQ.2) THEN
         IF (TYPE.EQ.0) THEN
            IF (ABS(XX).LE.1.D-5) THEN
               DSINCS = 1.0D0 - XX*XX / 6.0D0 + XX*XX*XX*XX / 120.0D0
            ELSE
               DSINCS = SIN (XX) / XX
               END IF
C                                       SIN(X)/X derivative
         ELSE IF (TYPE.EQ.1) THEN
            IF (ABS(XX).LE.1.D-5) THEN
               DSINCS = -XX/3.D0 + XX*XX*XX/30.D0 -
     *            XX*XX*XX*XX*XX/840.D0
            ELSE
               DSINCS = (XX * COS(XX) - SIN(XX)) / (XX*XX)
               END IF
            END IF
C                                       Gauss function
      ELSE IF (MODEL.EQ.3) THEN
         F = -LOG (2.0D0) / (1.8954 * 1.8954)
         IF (TYPE.EQ.0) THEN
            DSINCS = EXP (F * XX * XX)
C                                       Gauss derivative
         ELSE IF (TYPE.EQ.1) THEN
            DSINCS = 2.0 * XX * F * EXP (F * XX * XX)
            END IF
C                                       Exp function
      ELSE IF (MODEL.EQ.4) THEN
         F = -LOG (2.0D0) / 1.8954
         IF (TYPE.EQ.0) THEN
            DSINCS = EXP (F * ABS(XX))
C                                       Exp derivative
         ELSE IF (TYPE.EQ.1) THEN
            DSINCS = F * EXP (F * ABS(XX))
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE MODIMH
C-----------------------------------------------------------------------
C   MODIMH copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72, NOTTYP*2, CODES(6)*4,
     *   TCODES(3)*4
      INTEGER   LUN1, LUN2, IERR, I, NCOMP, J, TIME(3), DATE(3)
      INCLUDE 'MODIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NOTTYP /'CC'/
      DATA CODES /'POIN','GAUS','DISK','RECT','SPHE','EXPD'/
      DATA TCODES /'SLAB', 'GAUS', 'EXP '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
      DO 100 J = 1,NPOL
         CALL COPY (256, CATNEW(1,J), CATBLK)
C                                       Copy/open history file.
         IF (.NOT.DONEW) THEN
            CALL HISCOP (LUN1, LUN2, DISKI(J), DISKO(J), OLDCNO(J),
     *         NEWCNO(J), CATBLK, SCRTCH, BUFF, IERR)
            IF (IERR.GT.2) THEN
               WRITE (MSGTXT,1000) IERR, 'COPYING HI FILE', J
               CALL MSGWRT (6)
               GO TO 20
               END IF
C                                       New history
            IF (J.EQ.1) THEN
               CALL HENCO1 (TSKNAM, NAMEI(J), CLASI(J), SEQI(J),
     *            DISKI(J), LUN2, BUFF, IERR)
            ELSE IF (J.EQ.2) THEN
               CALL HENCO2 (TSKNAM, NAMEI(J), CLASI(J), SEQI(J),
     *            DISKI(J), LUN2, BUFF, IERR)
            ELSE
               CALL HENCO3 (TSKNAM, NAMEI(J), CLASI(J), SEQI(J),
     *            DISKI(J), LUN2, BUFF, IERR)
               END IF
            IF (IERR.NE.0) GO TO 20
C                                       BLC
            WRITE (HILINE,2000) TSKNAM, BLC
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       TRC
            WRITE (HILINE,2001) TSKNAM, TRC
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       need new HI file
         ELSE
C                                       Create/open hist. file.
            CALL HICREA (LUN2, DISKO(J), NEWCNO(J), CATBLK, BUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CREATING HI FILE', J
               CALL MSGWRT (6)
               GO TO 20
               END IF
C                                       Get current date/time.
            CALL ZDATE (DATE)
            CALL ZTIME (TIME)
            CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
            WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
            END IF
C                                       outname
         CALL HENCOO (TSKNAM, NAMOUT, CLASO(J), SEQO(J), DISKO(J), LUN2,
     *      BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       Components
         NCOMP = NGAUSS
         NCOMP = MAX (1, MIN (9, NCOMP))
         IF (NGAUSS.GT.NCOMP) THEN
            MSGTXT = 'ONLY FIRST 9 LISTED IN HI'
            IF (J.EQ.1) CALL MSGWRT (2)
            END IF
         WRITE (HILINE,2003) TSKNAM, NGAUSS
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20

         DO 10 I = 1,NCOMP
C                                       IONE
            WRITE (HILINE,2004) TSKNAM, I, IONE(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       QONE
            WRITE (HILINE,2005) TSKNAM, I, QONE(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       UONE
            WRITE (HILINE,2006) TSKNAM, I, UONE(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       SPIX
            WRITE (HILINE,2007) TSKNAM, I, SPIX(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       RM
            WRITE (HILINE,2008) TSKNAM, I, RM(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       RMTHICK
            WRITE (HILINE,2009) TSKNAM, I, RMTHIK(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       FPOS
            WRITE (HILINE,2010) TSKNAM, I, FPOS(1,I), FPOS(2,I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       BMAJ
            IF (ICODES(I).NE.1) THEN
               WRITE (HILINE,2011) TSKNAM, I, FWID(1,I), FWID(2,I),
     *            FWID(3,I)
               CALL HIADD (LUN2, HILINE, BUFF, IERR)
               IF (IERR.NE.0) GO TO 20
               END IF
C                                       OPCODE
            WRITE (HILINE,2012) TSKNAM, I, CODES(ICODES(I))
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
 10         CONTINUE
C                                       FLUX
         WRITE (HILINE,2020) TSKNAM, FLUX
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       FACTOR
         WRITE (HILINE,2021) TSKNAM, FACTOR
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       OPTYPE
         IF (SPIXDO.GT.0) THEN
            WRITE (HILINE,2022) TSKNAM, TCODES(SPIXDO-1)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
            END IF
C                                       Close HI file
 20      CALL HICLOS (LUN2, .TRUE., BUFF, IERR)
C                                        Copy tables
         IF (.NOT.DONEW) THEN
            CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKI(J), DISKO(J),
     *         OLDCNO(J), NEWCNO(J), CATBLK, BUFF, SCRTCH, IERR)
            IF (IERR.GT.2) THEN
               MSGTXT = 'ERROR COPYING TABLE FILES'
               CALL MSGWRT (6)
               END IF
            END IF
C                                        Update CATBLK.
         CALL CATIO ('UPDT', DISKO(J), NEWCNO(J), CATBLK, 'REST',
     *      BUFF, IERR)
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODIMH: ERROR',I3,' ON ',A,' FILE',I2)
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 2000 FORMAT (A6,'BLC =',7F6.0)
 2001 FORMAT (A6,'TRC =',7F6.0)
 2003 FORMAT (A6,'NGAUSS=',I6,'    total number components')
 2004 FORMAT (A6,'IONE(',I4,')  =',1PE12.4,12X,'/ JY/BEAM')
 2005 FORMAT (A6,'QONE(',I4,')  =',1PE12.4,12X,'/ JY/BEAM')
 2006 FORMAT (A6,'UONE(',I4,')  =',1PE12.4,12X,'/ JY/BEAM')
 2007 FORMAT (A6,'SPIX(',I4,')  =',1PE12.4,12X,'/ unitless')
 2008 FORMAT (A6,'RM(',I4,')    =',1PE12.4,12X,'/ RAD/M/M')
 2009 FORMAT (A6,'RMTHICK(',I4,')  =',F8.2,',',F8.2,7X,'/ RAD/M/M')
 2010 FORMAT (A6,'FPOS(',I4,')  =',F8.2,',',F8.2,7X,'/ pixels')
 2011 FORMAT (A6,'FWIDTH(',I4,') =',F7.3,',',F7.3,',',F6.1,
     *   '  / Maj pix, Min pix, PA deg')
 2012 FORMAT (A6,'OPCODE(',I4,') = ''',A4,'''',12X,'/ component type')
 2020 FORMAT (A6,'FLUX = ',1PE12.4,5X,'/ noise added')
 2021 FORMAT (A6,'FACTOR = ',1PE12.4,5X,'/ Applied to input data')
 2022 FORMAT (A6,'OPTYPE = ''',A4,'''',12X,'/ thickness model')
      END
