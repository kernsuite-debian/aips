LOCAL INCLUDE 'RFARS.INC'
      INCLUDE 'INCS:PMAD.INC'
      HOLLERITH XNAM1(3), XCLS1(2), XNAM2(3), XCLS2(2), XNAM3(3),
     *   XCLS3(2), ONAM1(3), OCLS1(2), ONAM2(3), OCLS2(2)
      REAL      XS1, XD1, XS2, XD2, XS3, XD3, OS1, OD1, OS2, OD2,
     *   BLC(7), TRC(7)
      COMMON /INPARM/ XNAM1, XCLS1, XS1, XD1, XNAM2, XCLS2, XS2, XD2,
     *   XNAM3, XCLS3, XS3, XD3, ONAM1, OCLS1, OS1, OD1, ONAM2, OCLS2,
     *   OS2, OD2, BLC, TRC
      CHARACTER NAMIN(3)*12, CLSIN(3)*6, NAMOUT(2)*12, CLSOUT(2)*6
      COMMON /CHPARM/ NAMIN, CLSIN, NAMOUT, CLSOUT
C
      INTEGER   CATSI(256,5)
      REAL      CATSR(256,5)
      HOLLERITH CATSH(256,5)
      DOUBLE PRECISION CATSD(128,5)
      EQUIVALENCE (CATSD, CATSH, CATSR, CATSI)
      INTEGER   SEQI(3), DISKI(3), SEQO(2), DISKO(2), SCRTCH(512),
     *   LUNI(3), LUNO(2), INDI(3), INDO(2), CNOI(3), CNOO(2), JBUFSZ
      DOUBLE PRECISION LAMBS(MAXIMG)
      REAL      BUFF1(MABFSS), BUFF2(MABFSS), BUFF3(MABFSS),
     *   BUFF4(MABFSS)
      COMMON /OTPARM/ CATSD, LAMBS, BUFF1, BUFF2, BUFF3, BUFF4, SCRTCH,
     *   SEQI, DISKI, SEQO, DISKO, LUNI, INDI, LUNO, INDO, CNOI, CNOO,
     *   JBUFSZ
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
LOCAL END
      PROGRAM RFARS
C-----------------------------------------------------------------------
C! Correct inputs to FARS for maximum RM found by AFARS from FARS output
C# POLARIZATION ANALYSIS
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2012
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
C   RFARS corrects the Q,U cubes read into FARS with an image of RM
C   usually made by AFARS from the outputs of FARS.
C   Inputs:
C       in 1 - Q
C       in 2 - U
C       in 3 - RM
C       out 1 - Q corrected
C       out 2 - U corrected
C       BLC for in 1 and 2
C       TRC for in 1 and 2
C   1 and 2 may be in freq-RA-Dec or RA-Dec-freq transpositions but
C   they must be the same
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET, ITYPE, NX, NY, NWORDS
      LONGINT   PRM
      REAL      RM(2)
      INCLUDE 'RFARS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'RFARS '/
C-----------------------------------------------------------------------
C                                       get inputs, create outputs
      CALL RFARSI (PRGNAM, ITYPE, NX, NY, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       memory for RM image
      NWORDS = (NX * NY - 1) / 1024 + 3
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, RM, PRM, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET DYNAMIC MEMORY FOR RM'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       read RM
      CALL RFARSR (ITYPE, NX, NY, RM(1+PRM), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       FREQ-RA-DEC
      IF (ITYPE.EQ.1) THEN
         CALL RFARSF (NX, NY, RM(1+PRM), IRET)
         IF (IRET.NE.0) GO TO 990
C                                       RA-DEC-FREQ
      ELSE
         CALL RFARSP (NX, NY, RM(1+PRM), IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       HI, etc
      CALL RFARSH
C
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE RFARSI (PRGNAM, ITYPE, NX, NY, IRET)
C-----------------------------------------------------------------------
C   RFARSI gets input parameters for RFARS and creates the output images
C   and reads the FREQ axis or FQ table to get the list of frequencies.
C   Inputs:
C      PRGNAM   C*6   task name
C   Outputs:
C      ITYPE    I     1 => freq or freqid axis first
C      NX       I     Number RA axis points in output
C      NY       I     Number Dec axis points in output
C      IRET     I     Error code
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   ITYPE, NX, NY, IRET
C
      INCLUDE 'RFARS.INC'
      INTEGER   IERR, NPARMS, I, IUSER
      CHARACTER INTYPE*2, CTEMP*8, DEFC(2)*6

      INCLUDE 'INCS:DFIL.INC'
      DATA DEFC /'RFARSQ','RFARSU'/
C-----------------------------------------------------------------------
C                                       init aips
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
C                                       get adverbs
      NPARMS = 49
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAM1, SCRTCH, IERR)
      IRET = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING ADVERB VALUES'
         CALL MSGWRT (8)
         END IF
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       convert adverbs
      CALL H2CHR (12, 1, XNAM1, NAMIN(1))
      CALL H2CHR (12, 1, XNAM2, NAMIN(2))
      CALL H2CHR (12, 1, XNAM3, NAMIN(3))
      CALL H2CHR (12, 1, ONAM1, NAMOUT(1))
      CALL H2CHR (12, 1, ONAM2, NAMOUT(2))
      CALL H2CHR (6, 1, XCLS1, CLSIN(1))
      CALL H2CHR (6, 1, XCLS2, CLSIN(2))
      CALL H2CHR (6, 1, XCLS3, CLSIN(3))
      CALL H2CHR (6, 1, OCLS1, CLSOUT(1))
      CALL H2CHR (6, 1, OCLS2, CLSOUT(2))
      SEQI(1) = XS1 + 0.1
      SEQI(2) = XS2 + 0.1
      SEQI(3) = XS3 + 0.1
      SEQO(1) = OS1 + 0.1
      SEQO(2) = OS2 + 0.1
      DISKI(1) = XD1 + 0.1
      DISKI(2) = XD2 + 0.1
      DISKI(3) = XD3 + 0.1
      DISKO(1) = OD1 + 0.1
      DISKO(2) = OD2 + 0.1
      LUNI(1) = 17
      LUNI(2) = 18
      LUNI(3) = 19
      LUNO(1) = 20
      LUNO(2) = 21
      JBUFSZ = MABFSS / 2
C                                       open inputs
      INTYPE = 'MA'
      IUSER = NLUSER
      DO 30 I = 1,3
         CALL MAPOPN ('READ', DISKI(I), NAMIN(I), CLSIN(I), SEQI(I),
     *      INTYPE, IUSER, LUNI(I), INDI(I), CNOI(I), CATSI(1,I),
     *      SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN INPUT IMAGE', I
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FRW(NCFILE) = 0
         FVOL(NCFILE) = DISKI(I)
         FCNO(NCFILE) = CNOI(I)
 30      CONTINUE
C                                       check window
      CALL WINDOW (CATSI(KIDIM,1), CATSI(KINAX,1), BLC, TRC, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       do first 2 match enough?
      IF ((CATSI(KIDIM,1).NE.CATSI(KIDIM,2)) .OR.
     *   (CATSI(KINAX,1).NE.CATSI(KINAX,2)) .OR.
     *   (CATSI(KINAX+1,1).NE.CATSI(KINAX+1,2)) .OR.
     *   (CATSI(KINAX+2,1).NE.CATSI(KINAX+2,2)) .OR.
     *   (CATSI(KINAX+3,1).NE.CATSI(KINAX+3,2))) THEN
         MSGTXT = 'Q AND U DO NOT MATCH IN HEADER PIXEL COUNTS'
         IRET = 10
         GO TO 990
         END IF
      IF ((CATSH(KHCTP,1).NE.CATSH(KHCTP,2)) .OR.
     *   (CATSH(KHCTP+1,1).NE.CATSH(KHCTP+1,2)) .OR.
     *   (CATSH(KHCTP+2,1).NE.CATSH(KHCTP+2,2)) .OR.
     *   (CATSH(KHCTP+3,1).NE.CATSH(KHCTP+3,2)) .OR.
     *   (CATSH(KHCTP+4,1).NE.CATSH(KHCTP+4,2)) .OR.
     *   (CATSH(KHCTP+5,1).NE.CATSH(KHCTP+5,2))) THEN
         MSGTXT = 'Q AND U DO NOT MATCH IN AXIS LABELS'
         IRET = 10
         GO TO 990
         END IF
C                                       close enough, what type?
      CALL H2CHR (8, 1, CATSH(KHCTP,1), CTEMP)
      IF ((CTEMP(:4).EQ.'FREQ') .OR. (CTEMP(:4).EQ.'FQID')) THEN
         ITYPE = 1
         NX = TRC(2) - BLC(2) + 1.1
         NY = TRC(3) - BLC(3) + 1.1
      ELSE
         CALL H2CHR (8, 1, CATSH(KHCTP+4,1), CTEMP)
         IF ((CTEMP(:4).EQ.'FREQ') .OR. (CTEMP(:4).EQ.'FQID')) THEN
            ITYPE = 2
            NX = TRC(1) - BLC(1) + 1.1
            NY = TRC(2) - BLC(2) + 1.1
         ELSE
            MSGTXT = 'NEITHER AXIS 1 NOR 3 IS FREQ OR FQID'
            IRET = 10
            GO TO 990
            END IF
         END IF
C                                       get frequencies
      CALL RFARSL (ITYPE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       create output files
      DO 50 I = 1,2
         CALL COPY (256, CATSI(1,I), CATBLK)
         CALL SUBHDR (BLC, TRC, 1.0, 1.0)
         CALL MAKOUT (NAMIN(I), CLSIN(I), SEQI(I), DEFC(I), NAMOUT(I),
     *      CLSOUT(I), SEQO(I))
         CALL CHR2H (12, NAMOUT(I), KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLSOUT(I), KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQO(I)
         CATBLK(KIIMU) = NLUSER
         CALL MCREAT (DISKO(I), CNOO(I), SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CREATING OUTPUT IMAGE', I
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FRW(NCFILE) = 2
         FVOL(NCFILE) = DISKO(I)
         FCNO(NCFILE) = CNOO(I)
C                                       copy header keywords
         CALL KEYPCP (DISKI(I), CNOI(I), DISKO(I), CNOO(I), 0, ' ',
     *      IERR)
         CALL COPY (256, CATBLK, CATSI(1,3+I))
C                                       open
         CALL MAPOPN ('INIT', DISKO(I), NAMOUT(I), CLSOUT(I), SEQO(I),
     *      INTYPE, IUSER, LUNO(I), INDO(I), CNOO(I), CATSI(1,3+I),
     *      SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT IMAGE', I
            GO TO 990
            END IF
 50      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFARSI: ERROR',I4,' ON ',A,I3)
      END
      SUBROUTINE RFARSL (ITYPE, IRET)
C-----------------------------------------------------------------------
C   RFARSL computes or reads the frequencies compute lambda**2
C   Inputs:
C      ITYPE   I   1 => x axis is freq, 2 => z axis
C   Outputs:
C      IRET    I   error code
C-----------------------------------------------------------------------
      INTEGER   ITYPE, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IBLC, ITRC, NZ, FQAX, I, J, VER, IFQRNO, FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC), NUMIF, IFSIDE, FREQAX, LUN, NUMREC, FQID
      CHARACTER CTEMP*4
      DOUBLE PRECISION IFFREQ
      REAL      IFCHW, IFTBW
      CHARACTER BNDCOD*8
      INCLUDE 'RFARS.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IF (ITYPE.EQ.1) THEN
         FQAX = 0
         IBLC = BLC(1) + 0.1
         ITRC = TRC(1) + 0.1
      ELSE
         FQAX = 2
         IBLC = BLC(3) + 0.1
         ITRC = TRC(3) + 0.1
         END IF
      NZ = ITRC - IBLC + 1
      CALL DFILL (NZ, 0.0D0, LAMBS)
C                                       frequency axis
      CALL H2CHR (4, 1, CATSH(KHCTP+2*FQAX,1), CTEMP)
      IF (CTEMP.NE.'FQID') THEN
         DO 20 I = IBLC,ITRC
            LAMBS(I-IBLC+1) = CATSD(KDCRV+FQAX,1) + CATSR(KRCIC+FQAX,1)
     *         * (I - CATSR(KRCRP+FQAX,1))
 20         CONTINUE
C                                       FREQ ID table
      ELSE
         CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), FREQAX,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'NO FREQUENCY AXIS FOUND: QUITTING'
            GO TO 990
            END IF
         VER = 1
         LUN = 31
         CALL FQINI ('READ', SCRTCH, DISKI(1), CNOI(1), VER, CATSI(1,1),
     *      LUN, IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING THE FQ TABLE'
            GO TO 990
            END IF
         NUMREC = SCRTCH(5)
         DO 40 I = 1,NUMREC
            CALL TABFQ ('READ', SCRTCH, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE'
               GO TO 990
               END IF
            J = (FQID - CATSD(KDCRV+FQAX,1)) / CATSR(KRCIC+FQAX,1) +
     *         CATSR(KRCRP+FQID,1)
            IF ((J.GE.IBLC) .AND .(J.LE.ITRC)) THEN
               J = J - IBLC + 1
               LAMBS(J) = IFFREQ + CATSD(KDCRV+FREQAX,1)
               END IF
 40         CONTINUE
         CALL TABFQ ('CLOS', SCRTCH, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
         IRET = 0
         END IF
C                                       convert to lambda*2
      DO 60 I = 1,NZ
         IF (LAMBS(I).LE.0.0D0) THEN
            IRET = 10
            MSGTXT = 'NOT ALL FREQUENCIES HAVE VALUE'
            GO TO 990
         ELSE
            LAMBS(I) = (VELITE / LAMBS(I)) ** 2
            END IF
 60      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFARSL: ERROR',I4,' ON ',A)
      END
      SUBROUTINE RFARSR (ITYPE, NX, NY, RM, IRET)
C-----------------------------------------------------------------------
C   RFARSR reads the RM image plane into memory
C   Inputs:
C      ITYPE   I      Type: 1 freq axis first
C      NX      I      Number X pixels in output
C      NY      I      Number Y pixels in output
C   Outputs
C      RM      R(*)   RM image
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   ITYPE, NX, NY, IRET
      REAL      RM(NX,*)
C
      INCLUDE 'RFARS.INC'
      INTEGER   IWIN(4), IBLKOF, IY, BIND
C-----------------------------------------------------------------------
      IF (ITYPE.EQ.1) THEN
         IWIN(1) = BLC(2) + 0.1
         IWIN(2) = BLC(3) + 0.1
         IWIN(3) = TRC(2) + 0.1
         IWIN(4) = TRC(3) + 0.1
      ELSE
         IWIN(1) = BLC(1) + 0.1
         IWIN(2) = BLC(2) + 0.1
         IWIN(3) = TRC(1) + 0.1
         IWIN(4) = TRC(2) + 0.1
         END IF
      IBLKOF = 1
      CALL MINIT ('READ', LUNI(3), INDI(3), NX, NY, IWIN, BUFF1, JBUFSZ,
     *   IBLKOF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO RM IMAGE'
         GO TO 990
         END IF
      DO 20 IY = 1,NY
         CALL MDISK ('READ', LUNI(3), INDI(3), BUFF1, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING RM IMAGE'
            GO TO 990
            END IF
         CALL RCOPY (NX, BUFF1(BIND), RM(1,IY))
 20      CONTINUE
C                                       done with RM image
      CALL ZCLOSE (LUNI(3), INDI(3), IY)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFARSR: ERROR',I4,' ON ',A)
      END
      SUBROUTINE RFARSF (NX, NY, RM, IRET)
C-----------------------------------------------------------------------
C   RFARSF does the correction for freq-RA-Dec images
C   Inputs:
C      NX     I      Number X pixels
C      NY     I      Number Y pixels
C      RM     R(*)   Image of rotation measure
C   Outputs:
C      IRET   I      Error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      RM(NX,*)
C
      INCLUDE 'RFARS.INC'
      INTEGER   IX, IY, I4, I5, I6, I7, N4, N5, N6, N7, IC, IBIND1, NC,
     *   IBIND2, OBIND1, OBIND2, IWIN(4), IBLKOF, IDEPTH(5), OWIN(4),
     *   INC, INX
      REAL      QI, UI, QO, UO, RI, SS, CC, RMAXQ, RMAXU, RMINQ, RMINU
      LOGICAL   WASBLK
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NC = TRC(1) - BLC(1) + 1.1
      N4 = TRC(4) - BLC(4) + 1.1
      N5 = TRC(5) - BLC(5) + 1.1
      N6 = TRC(6) - BLC(6) + 1.1
      N7 = TRC(7) - BLC(7) + 1.1
      IWIN(1) = BLC(1) + 0.1
      IWIN(2) = BLC(2) + 0.1
      IWIN(3) = TRC(1) + 0.1
      IWIN(4) = TRC(2) + 0.1
      OWIN(1) = 1
      OWIN(2) = 1
      OWIN(3) = NC
      OWIN(4) = NX
      INC = CATSI(KINAX,1)
      INX = CATSI(KINAX+1,1)
      RMAXQ = -1.E10
      RMINQ = 1.E10
      RMAXU = -1.E10
      RMINU = 1.E10
      WASBLK = .FALSE.
      DO 100 I7 = 1,N7
         DO 90 I6 = 1,N6
            DO 80 I5 = 1,N5
               DO 70 I4 = 1,N4
                  DO 60 IY = 1,NY
C                                       init reads of plane
                     IDEPTH(1) = IY + BLC(3) - 0.9
                     IDEPTH(2) = I4 + BLC(4) - 0.9
                     IDEPTH(3) = I5 + BLC(5) - 0.9
                     IDEPTH(4) = I6 + BLC(6) - 0.9
                     IDEPTH(5) = I7 + BLC(7) - 0.9
                     CALL COMOFF (CATSI(KIDIM,1), CATSI(KINAX,1),
     *                  IDEPTH, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'COMOFF INPUT'
                        GO TO 990
                        END IF
                     IBLKOF = IBLKOF + 1
                     CALL MINIT ('READ', LUNI(1), INDI(1), INC, INX,
     *                  IWIN, BUFF1, JBUFSZ, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT Q INPUT'
                        GO TO 990
                        END IF
                     CALL MINIT ('READ', LUNI(2), INDI(2), INC, INX,
     *                  IWIN, BUFF2, JBUFSZ, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT U INPUT'
                        GO TO 990
                        END IF
C                                       init writes
                     IDEPTH(1) = IY
                     IDEPTH(2) = I4
                     IDEPTH(3) = I5
                     IDEPTH(4) = I6
                     IDEPTH(5) = I7
                     CALL COMOFF (CATSI(KIDIM,4), CATSI(KINAX,4),
     *                  IDEPTH, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'COMOFF INPUT'
                        GO TO 990
                        END IF
                     IBLKOF = IBLKOF + 1
                     CALL MINIT ('WRIT', LUNO(1), INDO(1), NC, NX, OWIN,
     *                  BUFF3, JBUFSZ, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT Q OUTPUT'
                        GO TO 990
                        END IF
                     CALL MINIT ('WRIT', LUNO(2), INDO(2), NC, NX, OWIN,
     *                  BUFF4, JBUFSZ, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT U OUTPUT'
                        GO TO 990
                        END IF
                     DO 50 IX = 1,NX
                        CALL MDISK ('READ', LUNI(1), INDI(1), BUFF1,
     *                     IBIND1, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'READING INPUT Q IMAGE'
                           GO TO 990
                           END IF
                        CALL MDISK ('READ', LUNI(2), INDI(2), BUFF2,
     *                     IBIND2, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'READING INPUT U IMAGE'
                           GO TO 990
                           END IF
                        CALL MDISK ('WRIT', LUNO(1), INDO(1), BUFF3,
     *                     OBIND1, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITING OUTPUT Q IMAGE'
                           GO TO 990
                           END IF
                        CALL MDISK ('WRIT', LUNO(2), INDO(2), BUFF4,
     *                     OBIND2, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITING OUTPUT U IMAGE'
                           GO TO 990
                           END IF
                        DO 30 IC = 1,NC
                           QI = BUFF1(IBIND1+IC-1)
                           UI = BUFF2(IBIND2+IC-1)
                           RI = RM(IX,IY)
                           IF ((QI.EQ.FBLANK) .OR. (UI.EQ.FBLANK) .OR.
     *                        (RI.EQ.FBLANK)) THEN
                              QO = FBLANK
                              UO = FBLANK
                              WASBLK = .TRUE.
                           ELSE IF ((QI.EQ.0.0) .AND. (UI.EQ.0.0)) THEN
                              QO = 0.0
                              UO = 0.0
                              RMAXQ = MAX (QO, RMAXQ)
                              RMAXU = MAX (UO, RMAXU)
                              RMINQ = MIN (QO, RMINQ)
                              RMINU = MIN (UO, RMINU)
                           ELSE
                              RI = 2.0 * RI * LAMBS(IC)
                              SS = SIN (RI)
                              CC = COS (RI)
                              QO = QI * CC + UI * SS
                              UO = UI * CC - QI * SS
                              RMAXQ = MAX (QO, RMAXQ)
                              RMAXU = MAX (UO, RMAXU)
                              RMINQ = MIN (QO, RMINQ)
                              RMINU = MIN (UO, RMINU)
                              END IF
                           BUFF3(OBIND1+IC-1) = QO
                           BUFF4(OBIND2+IC-1) = UO
 30                        CONTINUE
 50                     CONTINUE
                     CALL MDISK ('FINI', LUNO(1), INDO(1), BUFF3,
     *                  OBIND1, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'FINISHING OUTPUT Q IMAGE'
                        GO TO 990
                        END IF
                     CALL MDISK ('FINI', LUNO(2), INDO(2), BUFF4,
     *                  OBIND2, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'FINISHING OUTPUT U IMAGE'
                        GO TO 990
                        END IF
 60                  CONTINUE
 70               CONTINUE
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       done
      CALL ZCLOSE (LUNI(1), INDI(1), I7)
      CALL ZCLOSE (LUNI(2), INDI(2), I7)
      CALL ZCLOSE (LUNO(1), INDO(1), I7)
      CALL ZCLOSE (LUNO(2), INDO(2), I7)
      CATSR(KRDMX,4) = RMAXQ
      CATSR(KRDMN,4) = RMINQ
      CATSR(KRDMX,5) = RMAXU
      CATSR(KRDMN,5) = RMINU
      IF (WASBLK) THEN
         CATSR(KRBLK,4) = FBLANK
         CATSR(KRBLK,5) = FBLANK
      ELSE
         CATSR(KRBLK,4) = 0.0
         CATSR(KRBLK,5) = 0.0
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFARSF: ERROR',I4,' ON ',A)
      END
      SUBROUTINE RFARSP (NX, NY, RM, IRET)
C-----------------------------------------------------------------------
C   RFARSP does the correction for RA-Dec-FREQ images
C   Inputs:
C      NX     I      Number X pixels
C      NY     I      Number Y pixels
C      RM     R(*)   Image of rotation measure
C   Outputs:
C      IRET   I      Error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      RM(NX,*)
C
      INCLUDE 'RFARS.INC'
      INTEGER   IX, IY, I4, I5, I6, I7, N4, N5, N6, N7, IC, IBIND1, NC,
     *   IBIND2, OBIND1, OBIND2, IWIN(4), IBLKOF, IDEPTH(5), OWIN(4),
     *   INX, INY
      REAL      QI, UI, QO, UO, RI, SS, CC, RMAXQ, RMAXU, RMINQ, RMINU
      LOGICAL   WASBLK
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NC = TRC(3) - BLC(3) + 1.1
      N4 = TRC(4) - BLC(4) + 1.1
      N5 = TRC(5) - BLC(5) + 1.1
      N6 = TRC(6) - BLC(6) + 1.1
      N7 = TRC(7) - BLC(7) + 1.1
      IWIN(1) = BLC(1) + 0.1
      IWIN(2) = BLC(2) + 0.1
      IWIN(3) = TRC(1) + 0.1
      IWIN(4) = TRC(2) + 0.1
      OWIN(1) = 1
      OWIN(2) = 1
      OWIN(3) = NX
      OWIN(4) = NY
      INX = CATSI(KINAX,1)
      INY = CATSI(KINAX+1,1)
      RMAXQ = -1.E10
      RMINQ = 1.E10
      RMAXU = -1.E10
      RMINU = 1.E10
      WASBLK = .FALSE.
      DO 100 I7 = 1,N7
         DO 90 I6 = 1,N6
            DO 80 I5 = 1,N5
               DO 70 I4 = 1,N4
                  DO 60 IC = 1,NC
C                                       init reads of plane
                     IDEPTH(1) = IC + BLC(3) - 0.9
                     IDEPTH(2) = I4 + BLC(4) - 0.9
                     IDEPTH(3) = I5 + BLC(5) - 0.9
                     IDEPTH(4) = I6 + BLC(6) - 0.9
                     IDEPTH(5) = I7 + BLC(7) - 0.9
                     CALL COMOFF (CATSI(KIDIM,1), CATSI(KINAX,1),
     *                  IDEPTH, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'COMOFF INPUT'
                        GO TO 990
                        END IF
                     IBLKOF = IBLKOF + 1
                     CALL MINIT ('READ', LUNI(1), INDI(1), INX, INY,
     *                  IWIN, BUFF1, JBUFSZ, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT Q INPUT'
                        GO TO 990
                        END IF
                     CALL MINIT ('READ', LUNI(2), INDI(2), INX, INY,
     *                  IWIN, BUFF2, JBUFSZ, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT U INPUT'
                        GO TO 990
                        END IF
C                                       init writes
                     IDEPTH(1) = IC
                     IDEPTH(2) = I4
                     IDEPTH(3) = I5
                     IDEPTH(4) = I6
                     IDEPTH(5) = I7
                     CALL COMOFF (CATSI(KIDIM,4), CATSI(KINAX,4),
     *                  IDEPTH, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'COMOFF INPUT'
                        GO TO 990
                        END IF
                     IBLKOF = IBLKOF + 1
                     CALL MINIT ('WRIT', LUNO(1), INDO(1), NX, NY, OWIN,
     *                  BUFF3, JBUFSZ, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT Q OUTPUT'
                        GO TO 990
                        END IF
                     CALL MINIT ('WRIT', LUNO(2), INDO(2), NX, NY, OWIN,
     *                  BUFF4, JBUFSZ, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT U OUTPUT'
                        GO TO 990
                        END IF
                     DO 50 IY = 1,NY
                        CALL MDISK ('READ', LUNI(1), INDI(1), BUFF1,
     *                     IBIND1, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'READING INPUT Q IMAGE'
                           GO TO 990
                           END IF
                        CALL MDISK ('READ', LUNI(2), INDI(2), BUFF2,
     *                     IBIND2, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'READING INPUT U IMAGE'
                           GO TO 990
                           END IF
                        CALL MDISK ('WRIT', LUNO(1), INDO(1), BUFF3,
     *                     OBIND1, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITING OUTPUT Q IMAGE'
                           GO TO 990
                           END IF
                        CALL MDISK ('WRIT', LUNO(2), INDO(2), BUFF4,
     *                     OBIND2, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITING OUTPUT U IMAGE'
                           GO TO 990
                           END IF
                        DO 30 IX = 1,NX
                           QI = BUFF1(IBIND1+IX-1)
                           UI = BUFF2(IBIND2+IX-1)
                           RI = RM(IX,IY)
                           IF ((QI.EQ.FBLANK) .OR. (UI.EQ.FBLANK) .OR.
     *                        (RI.EQ.FBLANK)) THEN
                              QO = FBLANK
                              UO = FBLANK
                              WASBLK = .TRUE.
                           ELSE IF ((QI.EQ.0.0) .AND. (UI.EQ.0.0)) THEN
                              QO = 0.0
                              UO = 0.0
                              RMAXQ = MAX (QO, RMAXQ)
                              RMAXU = MAX (UO, RMAXU)
                              RMINQ = MIN (QO, RMINQ)
                              RMINU = MIN (UO, RMINU)
                           ELSE
                              RI = 2.0 * RI * LAMBS(IC)
                              SS = SIN (RI)
                              CC = COS (RI)
                              QO = QI * CC + UI * SS
                              UO = UI * CC - QI * SS
                              RMAXQ = MAX (QO, RMAXQ)
                              RMAXU = MAX (UO, RMAXU)
                              RMINQ = MIN (QO, RMINQ)
                              RMINU = MIN (UO, RMINU)
                              END IF
                           BUFF3(OBIND1+IX-1) = QO
                           BUFF4(OBIND2+IX-1) = UO
 30                        CONTINUE
 50                     CONTINUE
                     CALL MDISK ('FINI', LUNO(1), INDO(1), BUFF3,
     *                  OBIND1, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'FINISHING OUTPUT Q IMAGE'
                        GO TO 990
                        END IF
                     CALL MDISK ('FINI', LUNO(2), INDO(2), BUFF4,
     *                  OBIND2, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'FINISHING OUTPUT U IMAGE'
                        GO TO 990
                        END IF
 60                   CONTINUE
 70                CONTINUE
 80             CONTINUE
 90          CONTINUE
 100     CONTINUE
C                                       done
      CALL ZCLOSE (LUNI(1), INDI(1), I7)
      CALL ZCLOSE (LUNI(2), INDI(2), I7)
      CALL ZCLOSE (LUNO(1), INDO(1), I7)
      CALL ZCLOSE (LUNO(2), INDO(2), I7)
      CATSR(KRDMX,4) = RMAXQ
      CATSR(KRDMN,4) = RMINQ
      CATSR(KRDMX,5) = RMAXU
      CATSR(KRDMN,5) = RMINU
      IF (WASBLK) THEN
         CATSR(KRBLK,4) = FBLANK
         CATSR(KRBLK,5) = FBLANK
      ELSE
         CATSR(KRBLK,4) = 0.0
         CATSR(KRBLK,5) = 0.0
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFARSP: ERROR',I4,' ON ',A)
      END
      SUBROUTINE RFARSH
C-----------------------------------------------------------------------
C   RFARSH writes history information to the output images
C   and copies non-CC tables
C-----------------------------------------------------------------------
      INCLUDE 'RFARS.INC'
      INTEGER   HLUN1, HLUN2, IERR, I, J, JTRIM, IC(7), INODIM
      CHARACTER HILINE*72, NOTTYP*2
      DATA HLUN1, HLUN2 /27, 28/
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
      CALL HIINIT (2)
C                                       2 output files
      DO 100 I = 1,2
         CALL COPY (256, CATSI(1,I+3), CATBLK)
         CALL HISCOP (HLUN1, HLUN2, DISKI(I), DISKO(I), CNOI(I),
     *      CNOO(I), CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.3) GO TO 100
         IF (I.EQ.1) THEN
            CALL HENCO1 (TSKNAM, NAMIN(I), CLSIN(I), SEQI(I), DISKI(I),
     *         HLUN2, BUFF2, IERR)
         ELSE
            CALL HENCO2 (TSKNAM, NAMIN(I), CLSIN(I), SEQI(I), DISKI(I),
     *         HLUN2, BUFF2, IERR)
            END IF
         IF (IERR.NE.0) GO TO 90
         CALL HENCO3 (TSKNAM, NAMIN(3), CLSIN(3), SEQI(3), DISKI(3),
     *      HLUN2, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 90
         CALL HENCOO (TSKNAM, NAMOUT(I), CLSOUT(I), SEQO(I), DISKO(I),
     *      HLUN2, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 90
C                                       corners
         INODIM = CATSI(KIDIM,I)
         DO 10 J = 1,INODIM
            IC(J) = BLC(J) + 0.5
 10         CONTINUE
         WRITE (HILINE,1010) TSKNAM, 'BLC', (IC(J), J = 1,INODIM)
         J = JTRIM (HILINE)
         IF (HILINE(J:J).EQ.',') HILINE(J:J) = ' '
         CALL HIADD (HLUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 90
         DO 15 J = 1,INODIM
            IC(J) = TRC(J) + 0.5
 15         CONTINUE
         WRITE (HILINE,1010) TSKNAM, 'TRC', (IC(J), J = 1,INODIM)
         J = JTRIM (HILINE)
         IF (HILINE(J:J).EQ.',') HILINE(J:J) = ' '
         CALL HIADD (HLUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 90
C                                       update header
  90     CALL HICLOS (HLUN2, .TRUE., BUFF2, IERR)
C                                       copy tables, not CC
 95      CALL ALLTAB (1, NOTTYP, HLUN1, HLUN2, DISKI(I), DISKO(I),
     *      CNOI(I), CNOO(I), CATBLK, BUFF1, BUFF2, IERR)
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A,A,' =',2(I6,','),4(I5,','),I5)
      END
