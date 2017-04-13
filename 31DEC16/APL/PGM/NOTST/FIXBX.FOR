LOCAL INCLUDE 'FIXBX.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      HOLLERITH XNAMEI(3), XCLASI(2), XXSOUR(4,30), XINFIL(12),
     *   XBXFIL(12), XOUFIL(12)
      REAL       DISKIN, XSEQ, XNOLD, ORASH(MAXAFL), ODECSH(MAXAFL),
     *   XNNEW, NRASH(MAXAFL), NDECSH(MAXAFL), XNBOX, CLBOX(4,50),
     *   CELSIZ(2), XSIZE(2), ROTATE, BPARM(10), XGAUSS
      COMMON /INPARM/ XNAMEI, XCLASI, XSEQ, DISKIN, XXSOUR, XINFIL,
     *   XNOLD, XGAUSS, ORASH, ODECSH, XNBOX, CLBOX, XBXFIL, XNNEW,
     *   NRASH, NDECSH, XOUFIL, CELSIZ, XSIZE, ROTATE, BPARM
C
      DOUBLE PRECISION RA0, DEC0, RAI(MAXFLD), DECI(MAXFLD),
     *   RAO(MAXFLD), DECO(MAXFLD)
      REAL      EDGSKP
      INTEGER   NFLDI, NFLDO, NGAUSS, WINI(4,MXNBFL), WINO(4,MXNBFL),
     *   IMSI(2,MAXFLD), IMSO(2,MAXFLD), NBOXI(MAXFLD), NBOXO(MAXFLD),
     *   IMSIZE(2), OLDSIZ(2), CATNO, SEQIN, IDISK, LUNBX, INDBX,
     *   SCRTCH(256)
      LOGICAL   RQUICK
      CHARACTER INFIL*48, BOXFIL*48, NAMEIN*12, CLASIN*6, OBOXF*48
      COMMON /FIXBXP/ RA0, DEC0, RAI, DECI, RAO, DECO, SCRTCH, NFLDI,
     *   NFLDO, WINI, WINO, IMSI, IMSO, NBOXI, NBOXO, IMSIZE, OLDSIZ,
     *   EDGSKP, CATNO, SEQIN, IDISK, LUNBX, RQUICK, INDBX, NGAUSS
      COMMON /FIXBXC/ INFIL, BOXFIL, OBOXF, NAMEIN, CLASIN
      INCLUDE 'INCS:DANT.INC'
LOCAL END
      PROGRAM FIXBX
C-----------------------------------------------------------------------
C! convert a BOXFILE to another for input to IMAGR
C# Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 2004-2005, 2008, 2011-2012
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
C   FIXBX makes a file which can then be used as an input BOXFILE
C   for IMAGR.  This is intended for wide-field imaging nd to cope with
C   interfering sources including the Sun.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME       XNAMEI/NAMEIN    Name of input UV data.
C      INCLASS      XCLASI/CLASIN    Class of input UV data.
C      INSEQ        SEQ/SEQIN        Seq. of input UV data.
C      INDISK       DISKIN/IDISK     Disk number of input UV data.
C      SOURCES      XXSOUR/SOURCE    Source which will be mapped.
C      BOXFILE      INFILE           old BOXFILE
C      OBOXFILE     OUFILE           new BOXFILE
C      CELLSIZE     CELSIZ           new pixel size in image.
C      IMSIZE       XSIZE/IMSIZE     new image size, also field size.
C      ROTATE       ROTATE           new image rotation
C      BPARM        BPARM            1. old CELLS(1)
C                                    2. old CELLS(2)
C                                    3. old IMSIZE(1)
C                                    4. old IMSIZE(2)
C                                    5. old ROTATE
C                                    6  edgskp
C-----------------------------------------------------------------------
      CHARACTER PNAM*6
C
      INTEGER   IRET
      INCLUDE 'FIXBX.INC'
      DATA PNAM /'FIXBX'/
C-----------------------------------------------------------------------
C                                       init
      CALL FIXBXI (PNAM, IRET)
C                                       do it
      IF (IRET.EQ.0) CALL FIXBXD (IRET)
C
      CALL DIETSK (IRET, RQUICK, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE FIXBXI (PNAM, IRET)
C-----------------------------------------------------------------------
C   starts up FIXBX, reads the input text files
C-----------------------------------------------------------------------
      CHARACTER PNAM*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER MTYPE*2, STAT*4
      INTEGER   NPARMS, IROUND, IBUFF(512), LUNSU, SQUAL, SOUID, NID,
     *   IERR, I, J, NF
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      HOLLERITH CATH(256)
      LOGICAL   MULTI
      INCLUDE 'FIXBX.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATH, CATD, CATR, CATBLK)
      DATA LUNSU /27/
      DATA NID, SQUAL /1, -1/
      DATA MTYPE /'UV'/
C-----------------------------------------------------------------------
C                                       initialize
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       get the input parameters
      NPARMS = 182 + 4 * MAXAFL + 200
      CALL GTPARM (PNAM, NPARMS, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         RQUICK = .FALSE.
         IF (IERR.EQ.1) THEN
            MSGTXT = 'CANNOT FIND INITIATOR IN GTPARM'
            CALL MSGWRT (1)
            GO TO 999
         ELSE
            MSGTXT= 'DISK PROBLEMS IN GTPARM'
            GO TO 980
            END IF
         END IF
      IRET = 0
      IF (RQUICK) CALL RELPOP (IRET, IBUFF, IERR)
C                                       set parameters
      IMSIZE(1) = IROUND (XSIZE(1))
      IMSIZE(2) = IROUND (XSIZE(2))
      IF (BPARM(1).LE.0.0) BPARM(1) = CELSIZ(1)
      IF (BPARM(2).LE.0.0) BPARM(2) = CELSIZ(2)
      IF (BPARM(3).LE.0.5) BPARM(3) = XSIZE(1)
      IF (BPARM(4).LE.0.5) BPARM(4) = XSIZE(2)
      OLDSIZ(1) = IROUND (BPARM(3))
      OLDSIZ(2) = IROUND (BPARM(4))
      EDGSKP = BPARM(6)
      IF (EDGSKP.LT.0.01) EDGSKP = 3.0
      BPARM(6) = EDGSKP
      IF ((BPARM(7).LT.0.1) .OR. (BPARM(7).GT.10.0)) BPARM(7) = 1.0
      CALL H2CHR (48, 1, XBXFIL, BOXFIL)
      CALL H2CHR (48, 1, XOUFIL, OBOXF)
      CALL H2CHR (48, 1, XINFIL, INFIL)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      SEQIN = IROUND (XSEQ)
      IDISK = IROUND (DISKIN)
C                                       locate map in directory
      CATNO = 1
      CALL CATDIR ('SRCH', IDISK, CATNO, NAMEIN, CLASIN, SEQIN,
     *   MTYPE, NLUSER, STAT, IBUFF, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'REQUESTED DATA NOT FOUND IN CATALOG DIRECTORY'
         GO TO 980
         END IF
C                                       read catalog block
      CALL CATIO ('READ', IDISK, CATNO, CATBLK, 'REST', IBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 980
         END IF
      WRITE (MSGTXT,1010) NAMEIN, CLASIN, SEQIN, IDISK, CATNO
      CALL MSGWRT (3)
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      UNAME = NAMEIN
      UCLAS = CLASIN
      UDISK = IDISK
      USEQ = SEQIN
C                                       single-source use UVPGET output
      CALL MULSDB (CATBLK, MULTI)
      IF (.NOT.MULTI) THEN
         SOUID = -1
         RA0 = RA
         DEC0 = DEC
         IF ((RA0.EQ.0.0D0) .AND. (DEC0.EQ.0.0D0)) THEN
            MSGTXT = 'CANNOT FIND SOURCE POSITION'
            GO TO 980
            END IF
C                                       multi-source get 1 source
      ELSE
         CALL H2CHR (16, 1, XXSOUR(1,1), SOURCS(1))
C                                       multi-source file, but no source
C                                       specified...
         IF (SOURCS(1).EQ.' ') THEN
            MSGTXT = 'MULTI SOURCE FILE BUT NO SOURCE SPECIFIED!'
            GO TO 980
C                                       find source...
         ELSE
            CALL SOURNU (SOURCS(1), SQUAL, 1, IDISK, CATNO, NID, IBUFF,
     *         SOUID, IRET)
            IF (IRET.EQ.0) THEN
               CALL GETSOU (SOUID, IDISK, CATNO, CATBLK, LUNSU, IRET)
               IF (IRET.EQ.0) THEN
                  DEC0 = DECEPO * RAD2DG
                  RA0 = RAEPO * RAD2DG
               ELSE
                  MSGTXT = 'ERROR ACCESSING SU EXTENSION'
                  GO TO 980
                  END IF
            ELSE
               MSGTXT = 'ERROR FINDING REQUESTED SOURCE'
               GO TO 980
               END IF
            END IF
         END IF
C                                       read box files for field parms
      CALL FIELDF (INFIL, RA0, DEC0, XNOLD, ORASH, ODECSH, BPARM(5),
     *   OLDSIZ, NF, NFLDI, IMSI, RAI, DECI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, BOXFIL
         GO TO 980
         END IF
      IF (NFLDI.LT.1) THEN
         NGAUSS = 1
         NFLDI = NF
      ELSE
         NGAUSS = XGAUSS + 0.1
         NGAUSS = MAX (1, NGAUSS)
         END IF
      IF (NFLDI.LT.1) THEN
         NFLDI = 1
         IMSI(1,1) = OLDSIZ(1)
         IMSI(2,1) = OLDSIZ(2)
         END IF
      CALL FIELDF (BOXFIL, RA0, DEC0, XNNEW, NRASH, NDECSH, ROTATE,
     *   IMSIZE, NF, NFLDO, IMSO, RAO, DECO, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.5)) THEN
         WRITE (MSGTXT,1100) IRET, OBOXF
         GO TO 980
         END IF
      IF (NFLDO.LT.1) NFLDO = NF
      IF (NFLDO.LT.1) THEN
         NFLDO = 1
         IMSO(1,1) = IMSIZE(1)
         IMSO(2,1) = IMSIZE(2)
         END IF
C                                       read windows
      I = NFLDI * NGAUSS
      CALL FILL (I, 0, NBOXI)
      IF (INFIL.NE.' ') THEN
         CALL WINDF (WINI, NBOXI, I, NGAUSS, IMSI, INFIL, .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) IRET, BOXFIL
            GO TO 980
            END IF
         END IF
      IF (NBOXI(1).LE.0) THEN
         NBOXI(1) = XNBOX + 0.5
         DO 10 I = 1,NBOXI(1)
            J = (I - 1) * NFLDI + 1
            WINI(1,J) = IROUND (CLBOX(1,I))
            WINI(2,J) = IROUND (CLBOX(2,I))
            WINI(3,J) = IROUND (CLBOX(3,I))
            WINI(4,J) = IROUND (CLBOX(4,I))
 10         CONTINUE
         END IF
      IF (NBOXI(1).LE.0) THEN
         NBOXI(1) = 1
         WINI(1,1) = 11
         WINI(2,1) = 11
         WINI(3,1) = OLDSIZ(1) - 10
         WINI(4,1) = OLDSIZ(2) - 10
         END IF
C                                       output windows
      I = NFLDO * NGAUSS
      CALL FILL (I, 0, NBOXO)
C     IF (BOXFIL.NE.' ') THEN
C        CALL WINDF (WINO, NBOXO, I, NGAUSS, IMSO, BOXFIL, .FALSE.,
C    *      IRET)
C        IF ((IRET.NE.0) .AND. (IRET.NE.5)) THEN
C           WRITE (MSGTXT,1110) IRET, OBOXF
C           GO TO 980
C           END IF
C        END IF
      IRET = 0
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATIO ERROR NO', I5)
 1010 FORMAT ('Found ', A12, A6, ' Seq', I4, ' Disk:', I3,' in slot',
     *   I5)
 1100 FORMAT ('ERROR',I5,' field reading ',A)
 1110 FORMAT ('ERROR',I5,' window reading ',A)
      END
      SUBROUTINE FIXBXD (IRET)
C-----------------------------------------------------------------------
C   Fixes the windows
C   Output:
C      IRET    I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, J1, J, NERR, IERR, LERR, LFERR, NTOT, K, K1, IROUND,
     *   NF(3), FF(10,3), IW(4), JJ, NG, KK
      REAL      DX, DY, TX(4), TY(4), EX, EY, XF(4,10,3), YF(4,10,3),
     *   CX, CY
      DOUBLE PRECISION TRA(4), TDEC(4), Z
      LOGICAL   CIRCLE
      INCLUDE 'FIXBX.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       mark as untouched
C     DO 10 I = 1,NFLDO
C        NBOXO(I) = -ABS (NBOXO(I))
C10      CONTINUE
C                                       Default output boxes
      DO 10 I = 1,NFLDO
         WINO(1,I) = -1
         WINO(2,I) = MIN (IMSO(1,I), IMSO(2,I)) / 2 - MAX (5., EDGSKP)
         WINO(3,I) = IMSO(1,I)/2
         WINO(4,I) = IMSO(2,I)/2 + 1
 10      CONTINUE
C                                       loop over input fields
      NTOT = 0
      LOCNUM = 1
      AXTYP(LOCNUM) = 1
      CORTYP(LOCNUM) = 1
      LABTYP(LOCNUM) = 65
      AXFUNC(1,LOCNUM) = 2
      AXFUNC(2,LOCNUM) = 2
      KLOCL(LOCNUM) = 0
      KLOCM(LOCNUM) = 1
      AXINC(1,LOCNUM) = -CELSIZ(1) / 3600.0
      AXINC(2,LOCNUM) = CELSIZ(2) / 3600.0
      ROT(LOCNUM) = ROTATE
      LOCNUM = 2
      AXTYP(LOCNUM) = 1
      CORTYP(LOCNUM) = 1
      LABTYP(LOCNUM) = 65
      AXFUNC(1,LOCNUM) = 2
      AXFUNC(2,LOCNUM) = 2
      KLOCL(LOCNUM) = 0
      KLOCM(LOCNUM) = 1
      AXINC(1,LOCNUM) = -BPARM(1) / 3600.0
      AXINC(2,LOCNUM) = BPARM(2) / 3600.0
      ROT(LOCNUM) = BPARM(5)
      COND2R = DG2RAD
      IRET = 0
      DO 100 I = 1,NFLDI
         DO 95 NG = 1,NGAUSS
            JJ = (NG - 1) * NFLDI + I
C                                       init location common
            LOCNUM = 2
            RPVAL(1,LOCNUM) = RAI(I)
            RPVAL(2,LOCNUM) = DECI(I)
            RPLOC(1,LOCNUM) = IMSI(1,I) / 2
            RPLOC(2,LOCNUM) = IMSI(2,I) / 2 + 1
C                                       loop over boxes
            DO 90 J = 1,NBOXI(JJ)
               IRET = IRET + 1
               NTOT = NTOT + 1
               LOCNUM = 2
               J1 = NGAUSS * NFLDI * (J - 1) + JJ
               CIRCLE = WINI(1,J1).EQ.-1
C                                       get corner coords
               IF (CIRCLE) THEN
                  DX = WINI(3,J1)
                  DY = WINI(4,J1) + WINI(2,J1) * BPARM(7)
                  CALL XYVAL (DX, DY, TRA(1), TDEC(1), Z, IERR)
                  DX = WINI(3,J1) - WINI(2,J1) * BPARM(7)
                  DY = WINI(4,J1)
                  CALL XYVAL (DX, DY, TRA(2), TDEC(2), Z, IERR)
                  DX = WINI(3,J1)
                  DY = WINI(4,J1) - WINI(2,J1) * BPARM(7)
                  CALL XYVAL (DX, DY, TRA(3), TDEC(3), Z, IERR)
                  DX = WINI(3,J1) + WINI(2,J1) * BPARM(7)
                  DY = WINI(4,J1)
                  CALL XYVAL (DX, DY, TRA(4), TDEC(4), Z, IERR)
               ELSE
                  CX = (WINI(1,J1) + WINI(3,J1)) / 2.0
                  CY = (WINI(2,J1) + WINI(4,J1)) / 2.0
                  DX = CX + BPARM(7) * (WINI(1,J1) - CX)
                  DY = CY + BPARM(7) * (WINI(4,J1) - CY)
                  CALL XYVAL (DX, DY, TRA(1), TDEC(1), Z, IERR)
                  DX = CX + BPARM(7) * (WINI(1,J1) - CX)
                  DY = CY + BPARM(7) * (WINI(2,J1) - CY)
                  CALL XYVAL (DX, DY, TRA(2), TDEC(2), Z, IERR)
                  DX = CX + BPARM(7) * (WINI(3,J1) - CX)
                  DY = CY + BPARM(7) * (WINI(2,J1) - CY)
                  CALL XYVAL (DX, DY, TRA(3), TDEC(3), Z, IERR)
                  DX = CX + BPARM(7) * (WINI(3,J1) - CX)
                  DY = CY + BPARM(7) * (WINI(4,J1) - CY)
                  CALL XYVAL (DX, DY, TRA(4), TDEC(4), Z, IERR)
                  END IF
C                                       loop over output fields
               LERR = 5
               NF(1) = 0
               NF(2) = 0
               NF(3) = 0
               DO 30 K = 1,NFLDO
                  KK = (NG - 1) * NFLDO + K
C                                       init location common
                  LOCNUM = 1
                  RPVAL(1,LOCNUM) = RAO(K)
                  RPVAL(2,LOCNUM) = DECO(K)
                  RPLOC(1,LOCNUM) = IMSO(1,K) / 2
                  RPLOC(2,LOCNUM) = IMSO(2,K) / 2 + 1
                  NERR = 0
                  CALL RFILL (4, -10000.0, TX)
                  CALL RFILL (4, -100000.0, TY)
                  DO 20 K1 = 1,4
                     CALL XYPIX (TRA(K1), TDEC(K1), DX, DY, IERR)
                     IF (IERR.NE.0) THEN
                        NERR = NERR + 4
                     ELSE IF ((DX.LT.EDGSKP+0.5) .OR. (DY.LT.EDGSKP+0.5)
     *                  .OR. (DX.GT.IMSO(1,K)-EDGSKP+0.5) .OR.
     *                  (DY.GT.IMSO(2,K)-EDGSKP+0.5)) THEN
                        TX(K1) = DX
                        TY(K1) = DY
                        NERR = NERR + 1
                     ELSE
                        TX(K1) = DX
                        TY(K1) = DY
                        END IF
 20                  CONTINUE
                  IF (NERR.LT.LERR) THEN
                     LERR = NERR
                     LFERR = K
                     END IF
C                                       put window in output
                  IF (NERR.EQ.0) THEN
                     NBOXO(KK) = MAX (0, NBOXO(KK)) + 1
                     K1 = (NBOXO(KK) - 1) * NFLDO * NGAUSS + KK
                     DX = MIN (TX(1), TX(2), TX(3), TX(4))
                     EX = MAX (TX(1), TX(2), TX(3), TX(4))
                     DY = MIN (TY(1), TY(2), TY(3), TY(4))
                     EY = MAX (TY(1), TY(2), TY(3), TY(4))
                     IF (CIRCLE) THEN
                        WINO(1,K1) = -1
                        WINO(2,K1) = (EX + EY - DX - DY) / 4.0  + 0.7
                        WINO(3,K1) = IROUND ((EX+DX)/2.0)
                        WINO(4,K1) = IROUND ((EY+DY)/2.0)
                     ELSE
                        WINO(1,K1) = IROUND (DX)
                        WINO(2,K1) = IROUND (DY)
                        WINO(3,K1) = IROUND (EX)
                        WINO(4,K1) = IROUND (EY)
                        END IF
C                                       remember possibles
                  ELSE IF ((NERR.LT.4) .AND. (NF(NERR).LT.10)) THEN
                     NF(NERR) = NF(NERR) + 1
                     FF(NF(NERR),NERR) = K
                     CALL RCOPY (4, TX, XF(1,NF(NERR),NERR))
                     CALL RCOPY (4, TY, YF(1,NF(NERR),NERR))
                     END IF
 30               CONTINUE
C                                       message rather than try for now
               IF (LERR.GT.0) THEN
                  WRITE (MSGTXT,1030) I, J, (WINI(K,J1), K = 1,4)
                  CALL MSGWRT (7)
                  K1 = 0
                  IF (NF(3).GT.0) K1 = 3
                  IF (NF(2).GT.0) K1 = 2
                  IF (NF(1).GT.0) K1 = 1
                  IF (K1.NE.0) THEN
                     DO 40 K = 1,NF(K1)
                        CALL RCOPY (4, XF(1,K,K1), TX)
                        CALL RCOPY (4, YF(1,K,K1), TY)
                        DX = MIN (TX(1), TX(2), TX(3), TX(4))
                        EX = MAX (TX(1), TX(2), TX(3), TX(4))
                        DY = MIN (TY(1), TY(2), TY(3), TY(4))
                        EY = MAX (TY(1), TY(2), TY(3), TY(4))
                        IW(1) = IROUND (DX)
                        IW(2) = IROUND (DY)
                        IW(3) = IROUND (EX)
                        IW(4) = IROUND (EY)
                        WRITE (MSGTXT,1035) FF(K,K1), IW
                        CALL MSGWRT (6)
 40                     CONTINUE
                     END IF
               ELSE
                  IRET = IRET - 1
                  END IF
 90            CONTINUE
 95         CONTINUE
 100     CONTINUE
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET, NTOT
         CALL MSGWRT (7)
      ELSE
         WRITE (MSGTXT,1101) NTOT
         CALL MSGWRT (3)
         END IF
C                                       copy to output
      CALL FIXBXO (BOXFIL, OBOXF, NFLDO*NGAUSS, NBOXO, WINO, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('FIELD',I5,' BOX',I5,' (',4I6,') NOT FOUND')
 1035 FORMAT ('   BUT new field',I5,' had corners',4I7)
 1100 FORMAT (I6,' BOXES OF',I7,' NOT COPIED TO OUTPUT')
 1101 FORMAT ('All',I7,' boxes copied to output')
      END
      SUBROUTINE FIXBXO (IBOXF, OBOXF, NFL, NBOX, WIN, IRET)
C-----------------------------------------------------------------------
C   Copies the control cards from IBOXF to OBOXF and then adds the
C   windows.
C   Inputs:
C      IBOXF   C*(*)   Input box file
C      OBOXF   C*(*)   Output box file
C      NFL     I       Number of fields
C      NBOX    I(*)    Number of boxes / field
C      WIN     I(*)    Windows(4,nfl,*)
C   Outputs
C      IRET    I       Error code
C-----------------------------------------------------------------------
      CHARACTER IBOXF*(*), OBOXF*(*)
      INTEGER   NFL, NBOX(*), WIN(4,NFL,*), IRET
C
      INTEGER   LUNI, INDI, LUNO, INDO, I, J, KBP, BX(4), IROUND, JTRIM,
     *   K
      REAL      RDUM
      DOUBLE PRECISION X
      CHARACTER LINE*132
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       open files
      LUNO = 11
      CALL ZTXOPN ('WRIT', LUNO, INDO, OBOXF, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT'
         GO TO 980
         END IF
      LUNI = 10
      IRET = 5
      IF (IBOXF.NE.' ') THEN
         CALL ZTXOPN ('READ', LUNI, INDI, IBOXF, .FALSE., IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.5)) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN INPUT'
            GO TO 980
            END IF
         END IF
C                                       copy non-box lines
      IF (IRET.NE.5) THEN
 10      CALL ZTXIO ('READ', LUNI, INDI, LINE, IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.2)) THEN
            WRITE (MSGTXT,1000) IRET, 'READ INPUT'
            GO TO 980
         ELSE IF (IRET.NE.2) THEN
            I = JTRIM (LINE)
            J = 0
            IF ((LINE(:1).NE.' ') .AND. (LINE(:1).GE.'0') .AND.
     *         (LINE(:1).LE.'9')) THEN
               KBP = 1
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.NE.DBLANK) THEN
                  RDUM = X
                  J = IROUND (RDUM)
                  END IF
               END IF
            IF ((J.LT.1) .OR. (J.GT.NFL)) THEN
               CALL ZTXIO ('WRIT', LUNO, INDO, LINE(:I), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'COPY TO OUTPUT'
                  GO TO 980
                  END IF
               END IF
            GO TO 10
            END IF
         CALL ZTXCLS (LUNI, INDI, IRET)
         END IF
C                                       write windows
      DO 50 I = 1,NFL
         K = 0
         DO 40 J = 1,MAX(1,NBOX(I))
            CALL COPY (4, WIN(1,I,J), BX)
            IF ((BX(1).NE.0) .AND. (BX(2).GT.0) .AND. (BX(3).GT.0) .AND.
     *         (BX(4).GT.0)) THEN
               K = K + 1
               WRITE (LINE,1010) I, BX
               CALL CHTRIM (LINE, 32, LINE, KBP)
               CALL ZTXIO ('WRIT', LUNO, INDO, LINE(:KBP), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE WINDOW'
                  GO TO 980
                  END IF
               END IF
 40         CONTINUE
         IF (K.EQ.0) THEN
            WRITE (MSGTXT,1005) I
            CALL MSGWRT (6)
            END IF
 50      CONTINUE
      CALL ZTXCLS (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' DOING ',A)
 1005 FORMAT ('OUTPUT FIELD',I5,' HAS NO CLEAN BOXES')
 1010 FORMAT (I4,4I7)
      END
      SUBROUTINE FIELDF (BOXFIL, RA, DEC, XNF, RASH, DECSH, MROTAT,
     *   IMSIZE, NF, NFIELD, FLDSIZ, RAS, DECS, IERR)
C-----------------------------------------------------------------------
C   Fills field parameters from the user test file F lines
C   Local version for finding # fields, RAs, DECs rather than shifts
C   Inputs:
C      BOXFIL   C*48     User provided file name containing box defs
C                        ' ' => none which is okay
C      RA       D        UV data set RA in degrees
C      DEC      D        UV data set Dec in degrees
C      XNF      R        Number of shifts to start with
C      RASH     R(*)     RAshift asec
C      DECSH    R(*)     DECshift asec
C      MROTAT   R        Total rotation in degrees CCW from North
C      IMSIZE   I(2)     Basic image size
C   In/Out:
C      NFIELD   I        Number of fields defined for field parms
C      NF       I        Number of fields with field or Clean lines
C      FLDSIZ   I(2,*)   image sizes
C      RAS      D(*)     Center RAs
C      DECS     D(*)     Center Declinations
C   Outputs:
C      IERR     I        Error return code: 0 => no error
C-----------------------------------------------------------------------
      CHARACTER BOXFIL*48
      DOUBLE PRECISION RA, DEC, RAS(*), DECS(*)
      INTEGER   NFIELD, NF, IMSIZE(2), FLDSIZ(2,*), IERR
      REAL      XNF, RASH(*), DECSH(*), MROTAT
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUN, I, J, FIND, KBP, I1, I2, NPTWO
      CHARACTER LINE*132, SIGN*1
      REAL      ARASH, ADECSH
      DOUBLE PRECISION X, XRA, XDEC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initial parameters
      NFIELD = XNF + 0.5
      IF (NFIELD.LE.0) THEN
         NFIELD = 0
         RAS(1) = RA
         DECS(1) = DEC
         FLDSIZ(1,1) = IMSIZE(1)
         FLDSIZ(2,1) = IMSIZE(2)
      ELSE
         DO 10 I1 = 1,NFIELD
            CALL XYSHFT (RA, DEC, RASH(I1), DECSH(I1), MROTAT, RAS(I1),
     *         DECS(I1))
            FLDSIZ(1,I1) = IMSIZE(1)
            FLDSIZ(2,I1) = IMSIZE(2)
 10         CONTINUE
         END IF
C                                        Open clean box file
      IF (BOXFIL.NE.' ') THEN
         LUN = 11
         CALL ZTXOPN ('READ', LUN, FIND, BOXFIL, .FALSE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                        Enter parameters from file
         I2 = 0
         NF = 0
         DO 50 I = 1,500000
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.EQ.2) GO TO 60
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN'
               CALL MSGWRT (6)
               GO TO 999
               END IF
            CALL CHTRIM (LINE, 132, LINE, J)
C                                       parse for field:
C                                       #, fldsiz, rash, decsh
            IF ((LINE(:1).EQ.'F') .OR. (LINE(:1).EQ.'f')) THEN
               KBP = 2
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 50
               IF (X.GE.0.0D0) THEN
                  I1 = X + 0.50D0
               ELSE
                  I1 = X - 0.50D0
                  END IF
               IF ((I1.GE.1) .AND. (I1.LE.MAXFLD)) THEN
                  NF = MAX (NF, I1)
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  FLDSIZ(1,I1) = X + 0.50D0
                  IF (X.LT.0.0D0) FLDSIZ(1,I1) = -1
                  FLDSIZ(1,I1) = MAX (FLDSIZ(1,I1), IMSIZE(1))
                  FLDSIZ(1,I1) = NPTWO (FLDSIZ(1,I1))
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  FLDSIZ(2,I1) = X + 0.50D0
                  IF (X.LT.0.0D0) FLDSIZ(2,I1) = -1
                  FLDSIZ(2,I1) = MAX (FLDSIZ(2,I1), IMSIZE(2))
                  FLDSIZ(2,I1) = NPTWO (FLDSIZ(2,I1))
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  ARASH = X
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  ADECSH = X
                  CALL XYSHFT (RA, DEC, ARASH, ADECSH, MROTAT, RAS(I1),
     *               DECS(I1))
                  I2 = I2 + 1
                  END IF
C                                       parse for ra/dec field
C                                       # fldsiz ra (HH MM SS) Dec (+-
C                                       DD MM SS)
            ELSE IF ((LINE(:1).EQ.'C') .OR. (LINE(:1).EQ.'c')) THEN
               KBP = 2
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 50
               IF (X.GE.0.0D0) THEN
                  I1 = X + 0.50D0
               ELSE
                  I1 = X - 0.50D0
                  END IF
               IF ((I1.GE.1) .AND. (I1.LE.MAXFLD)) THEN
                  NF = MAX (NF, I1)
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  FLDSIZ(1,I1) = X + 0.50D0
                  FLDSIZ(1,I1) = MAX (FLDSIZ(1,I1), IMSIZE(1))
                  FLDSIZ(1,I1) = NPTWO (FLDSIZ(1,I1))
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  FLDSIZ(2,I1) = X + 0.50D0
                  FLDSIZ(2,I1) = MAX (FLDSIZ(2,I1), IMSIZE(2))
                  FLDSIZ(2,I1) = NPTWO (FLDSIZ(2,I1))
C                                       RA
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XRA = X
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XRA = XRA + X/60.0D0
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XRA = XRA + X/3600.0D0
                  XRA = XRA * 15.0D0
C                                       Sign
                  J = KBP
 40               IF (J.GT.132) GO TO 990
                  IF (LINE(J:J).EQ.' ') THEN
                     J = J + 1
                     GO TO 40
                     END IF
                  SIGN = LINE(J:J)
C                                       DEC
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XDEC = ABS(X)
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XDEC = XDEC + X/60.0D0
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  XDEC = XDEC + X/3600.0D0
                  IF (SIGN.EQ.'-') XDEC = -XDEC
                  RAS(I1) = XRA
                  DECS(I1) = XDEC
                  I2 = I2 + 1
                  END IF
            ELSE IF ((LINE(:1).GE.'0') .AND. (LINE(:1).LE.'9')) THEN
               KBP = 1
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 50
               IF (X.GE.0.0D0) THEN
                  I1 = X + 0.50D0
               ELSE
                  I1 = X - 0.50D0
                  END IF
               IF ((I1.GE.1) .AND. (I1.LE.MAXFLD)) NF = MAX (NF, I1)
               END IF
 50         CONTINUE
C
 60      CALL ZTXCLS (LUN, FIND, I)
         END IF
      IERR = 0
      GO TO 999
C
 990  WRITE (MSGTXT,1990) I
      CALL MSGWRT (6)
      IERR = 1

 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIELDF: ERROR',I4,1X,A,'ING BOXFILE')
 1990 FORMAT ('FIELDF: PARSING ERROR ON LINE',I4)
      END
      INTEGER FUNCTION NPTWO (N)
C-----------------------------------------------------------------------
C   Function to determine the next highest power of two for an integer.
C   Input:
C      N       I  Integer
C   Output:
C      NPTWO   I  Next highest power of two
C-----------------------------------------------------------------------
      INTEGER   N
C
      INTEGER   IPOW
      REAL      POW
C-----------------------------------------------------------------------
      POW = LOG (1.0*N) / LOG (2.0) + 0.99999
      IPOW = POW
      NPTWO = 2 ** IPOW
C
 999  RETURN
      END
      SUBROUTINE DTRANS (ABSVAL, DEGHR, MINUTE, SECOND)
C-----------------------------------------------------------------------
C   translates coordinate into sexagesimal
C   Inputs:
C      ABSVAL   D   Coordinate (with sign)
C   Outputs:
C      DEGHR    I   Degrees (or hours if ABSVAL so scaled) with sign
C      MINUTE   I   Minutes (no sign)
C      SECOND   I   Seconds (no sign)
C-----------------------------------------------------------------------
      DOUBLE PRECISION ABSVAL
      INTEGER   DEGHR, MINUTE
C
      DOUBLE PRECISION SECOND, VALCOP
C-----------------------------------------------------------------------
      VALCOP = ABSVAL
      DEGHR = INT (VALCOP)
      VALCOP = 60.0D0 * DABS (VALCOP - DEGHR)
      MINUTE = INT (VALCOP)
      SECOND = 60.0D0 * (VALCOP - MINUTE)
C
 999  RETURN
      END
      SUBROUTINE WINDF (WIN, NBOXES, MFIELD, NGAUSS, IMSIZE, BOXFIL,
     *   PRT, IERR)
C-----------------------------------------------------------------------
C   Fills the WIN array with clean box definitions taken from BOXFIL
C   Inputs:
C      BOXFIL   C*48        User provided file name containing box defs
C      MFIELD   I           Number of fields defined
C      NGAUSS   I           Number Gaussians
C      PRT      L           Do printing?
C   In/Out:
C      WIN      I(4,*,*)    clean boxes - defaulted on in (4,FIELD,BOX)
C      NBOXES   I*(*)       Array containing number of boxes/field
C   Outputs:
C      IERR     I           Error return code:
C                              0 => no error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER BOXFIL*48
      INTEGER   MFIELD, NGAUSS, WIN(4,MFIELD,*), NBOXES(*), IMSIZE(2,*),
     *   IERR
      LOGICAL   PRT
C
      INTEGER   LUN, I, J, ISUBF(MXNBOX), IFIELD, FIND, IPARM(5), KBP,
     *   I1, I2, IDD, LIMIT, JFIELD, NFIELD
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LIMIT = MIN (MXNBOX, MXNBFL / MAX(1,MFIELD))
C                                        Open clean box file
      LUN = 11
      CALL ZTXOPN ('READ', LUN, FIND, BOXFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                        Enter box parameters from file
      CALL FILL (MXNBOX, 0, ISUBF)
      IDD = 0
      DO 50 I = 1,100000
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.EQ.2) GO TO 60
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       check for comments
         CALL CHTRIM (LINE, 132, LINE, J)
         IF (LINE(:1).LT.'0') GO TO 50
         IF (LINE(:1).GT.'9') GO TO 50
C                                       parse for 5 integers
C                                       field, blc, trc
         KBP = 1
         CALL FILL (5, 0, IPARM)
         DO 30 J = 1,5
            CALL GETNUM (LINE, 132, KBP, X)
            IF (X.EQ.DBLANK) THEN
               IF (J.EQ.1) GO TO 50
               WRITE (MSGTXT,1020) I, J
               CALL MSGWRT (6)
               IERR = 1
               GO TO 999
            ELSE
               IF (X.GE.0.0D0) THEN
                  IPARM(J) = X + 0.50D0
               ELSE
                  IPARM(J) = X - 0.50D0
                  END IF
               END IF
 30         CONTINUE
         IFIELD = IPARM(1)
         NFIELD = MFIELD / NGAUSS
         JFIELD = MOD (IFIELD-1, NFIELD) + 1
         IF ((IFIELD.GE.1) .AND. (IFIELD.LE.MFIELD)) THEN
            IDD = IDD + 1
            ISUBF(IFIELD) = ISUBF(IFIELD) + 1
            IF (ISUBF(IFIELD).LE.LIMIT) THEN
               J = ISUBF(IFIELD)
C                                       circular
               IF (IPARM(2).LT.0) THEN
                  WIN(1,IFIELD,J) = -1
                  WIN(3,IFIELD,J) = MAX (1+IPARM(3), MIN (IPARM(4),
     *               IMSIZE(1,JFIELD)-IPARM(3)))
                  WIN(4,IFIELD,J) = MAX (1+IPARM(3), MIN (IPARM(5),
     *               IMSIZE(2,JFIELD)-IPARM(3)))
                  WIN(2,IFIELD,J) = IPARM(3)
C                                       rectangular
               ELSE
                  WIN(1,IFIELD,J) = MAX (1, MIN (IPARM(2),
     *               IMSIZE(1,JFIELD)))
                  WIN(3,IFIELD,J) = MAX (1, MIN (IPARM(4),
     *               IMSIZE(1,JFIELD)))
                  IF (WIN(3,IFIELD,J).LT.WIN(1,IFIELD,J)) THEN
                     I1 = WIN(3,IFIELD,J)
                     WIN(3,IFIELD,J) = WIN(1,IFIELD,J)
                     WIN(1,IFIELD,J) = I1
                     END IF
                  WIN(2,IFIELD,J) = MAX (1, MIN (IPARM(3),
     *               IMSIZE(2,JFIELD)))
                  WIN(4,IFIELD,J) = MAX (1, MIN (IPARM(5),
     *               IMSIZE(2,JFIELD)))
                  IF (WIN(4,IFIELD,J).LT.WIN(2,IFIELD,J)) THEN
                     I1 = WIN(4,IFIELD,J)
                     WIN(4,IFIELD,J) = WIN(2,IFIELD,J)
                     WIN(2,IFIELD,J) = I1
                     END IF
                  END IF
               END IF
            END IF
 50      CONTINUE
 60   DO 70 I = 1,MFIELD
         IF (ISUBF(I).GT.0) NBOXES(I) = MIN (LIMIT, ISUBF(I))
         IF (ISUBF(I).GT.LIMIT) THEN
            WRITE (MSGTXT,1060) I, LIMIT, ISUBF(I)
            CALL MSGWRT (6)
            END IF
 70      CONTINUE
      CALL ZTXCLS (LUN, FIND, I)
      IERR = 0
C
      IF ((IDD.GT.0) .AND. (PRT)) THEN
         MSGTXT = 'WINDF: Number of clean boxes/field read as follows:'
         CALL MSGWRT(7)
         DO 80 J = 1,MFIELD,4
            I1 = J
            I2 = MIN (MFIELD, I1+3)
            WRITE (MSGTXT,1070) ('Field', I, NBOXES(I), I = I1,I2)
            CALL MSGWRT (3)
 80         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WINDF: ERROR',I4,1X,A,'ING THE CLEAN BOXES TEXT FILE')
 1020 FORMAT ('WINDF: PARSING ERROR ON LINE',I4,' FIELD',I2)
 1060 FORMAT ('WARNING: FIELD',I4,' USED FIRST',I5,' OF',I6,
     *   ' BOXES IN FILE')
 1070 FORMAT (4(A5,I3.2,':',I4,2X))
      END
