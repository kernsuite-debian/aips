      SUBROUTINE SETGDS (UVDISK, UVCNO, MODNAM, MODCLS, MODSEQ, MODVOL,
     *   NMOD, VER, NITER, BITER, MODEL, METHOD, BUFFER, IBUFF, ISTOKE,
     *   IERR)
C-----------------------------------------------------------------------
C! Sets up for UV model computation, fills common in DGDS.INC
C# UV Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2002, 2004-2005, 2008-2010, 2012
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
C   SETGDS fills in the common /MAPDES/ (include DGDS.INC) needed before
C   calling UVMDIV or UVMSUB.  Mostly it finds the catalog slot numbers
C   of the model files.  The name, class of the first file are given
C   explicitly; the subsequent files are specified by the same name,
C   sequence, disk etc and the same class except for the 5th and 6th
C   characters which will be '01','02'...'15' for the 2nd thru MAXFLD
C   files.  Files are marked 'READ' in catalog and the /CFILES/ common.
C   The DO3DIM parameter is set by comparing image header parameters
C   with the UV data header (include DUVH.INC set by UVPGET before
C   calling SETGDS).  It also resets the MAXABSU parameter if any in
C   the UV data header - this forces UVPROT to recompute it once with
C   the current set of model images.
C   Inputs:
C      UVDISK   I         Disk number of main UV file <= 0 => ignore
C      UVCNO    I         Catalog number of main UV file
C      MODNAM   C*12      Name of model file(s).
C      MODCLS   C*6       Class of first model file.
C      MODSEQ   I         Model file(s) sequence number.
C      MODVOL   I         Model file(s) disk number
C      NMOD     I         Number of model files
C      VER      I         CC file(s) version number
C      BITER    I(*)      Number of components to ignore per field.
C      METHOD   I         Modeling method, -1=>DFT, 0=>chose, 1=>gridded
C      BUFFER   R(1024)   Work buffer
C      IBUFF    I(512)    Work buffer
C   In/out:
C      MODEL    I         Model type 0 => choose, 1=>CC, 2=>image
C                            3 => subimage Changed if CC files not found
C      NITER    I(*)      Maximum number of components per field - out =
C                         actual number to be used.
C                         NITER(I) < 0 -> max (field(i))
C                         NITER(I) = = -> max (i) only if all NITER 0
C      METHOD   I         Modeling method, -1=>DFT, 0=>chose, 1=>gridded
C   Output:
C      ISTOKE   I         Stokes' type of first model file.
C      IERR     I         Return code, 0=>OK, otherwise failed.
C                            3 => CC requested and not found
C                            2 => status, 4 => I/O, 6 => open fails
C                            5 => mixed epoch
C-----------------------------------------------------------------------
      CHARACTER MODNAM*12, MODCLS*6
      INTEGER   UVDISK, UVCNO, MODSEQ, MODVOL, NMOD, VER, NITER(*),
     *   BITER(*), MODEL, METHOD, IBUFF(*), ISTOKE, IERR
      REAL      BUFFER(*)
C
      CHARACTER STOKES*8, CLAT*6, ITYPE*2, STAT*4, CHTEMP*8
      HOLLERITH CATCH(256)
      INTEGER   I, LUNC, CATCLN(256), ISOFF, JNREC, JLREC, NKEY, SUM2,
     *   I4T, NCEN, NOFF, ONCE, IRET, J0, JJ, LOCS(2), VALUES(2),
     *   KEYTYP(2), IRAOFF, IDECOF, ONCED, ONCEC, J, K, MX, MY, MXOFF,
     *   MYOFF, MSGSAV
      REAL      CATCR(256), PMAT(3,3), MAXBLN, UMATN(3,3)
      DOUBLE PRECISION CATCD(128), CLNRA, CLNDEC
      LOGICAL   WASOME, OLDNAM, FORCED, LFORCE, DOUMAT, WASDIF, ISUV,
     *   WASOFF, IN3D, FIRST
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INTEGER   TMPFLD(4,MAXFLD)
      EQUIVALENCE (CATCLN, CATCR, CATCH, CATCD)
      SAVE ONCE, ONCED, ONCEC
      DATA LUNC, ONCE, ONCED, ONCEC /29, -1, -1, -1/
      DATA STOKES /'STOKES  '/
C-----------------------------------------------------------------------
      FIRST = .TRUE.
      MSGSAV = MSGSUP
      WASDIF = .FALSE.
      IF ((UVDISK.LE.0) .OR. (UVCNO.LE.0)) THEN
         DOUMAT = .FALSE.
         UVROT = 0.0
      ELSE
         DOUMAT = (UVDISK.NE.ONCED) .OR. (UVCNO.NE.ONCEC)
         ONCED = UVDISK
         ONCEC = UVCNO
         END IF
      IN3D = DO3DIM
C                                       header of main UV: get rot
      IF (DOUMAT) THEN
         CALL CATIO ('READ', UVDISK, UVCNO, CATCLN, 'REST', BUFFER,
     *      IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.6)) THEN
            IERR = 4
            IF (IRET.GT.4) IERR = 2
            WRITE (MSGTXT,1015) IRET, UVDISK, UVCNO
            GO TO 990
            END IF
         CALL ROTFND (CATCR, UVROT, IRET)
         END IF
C                                       Other info
      MFIELD = NMOD
      SUM2 = 0
      CLAT = MODCLS
      NCEN = 0
      NOFF = 0
      IERR = 0
      WASOME = .FALSE.
      OLDNAM = (CLAT(4:4).LT.'0') .OR. (CLAT(4:4).GT.'9') .OR.
     *   (CLAT(5:5).LT.'0') .OR. (CLAT(5:5).GT.'9') .OR.
     *   (CLAT(6:6).LT.'0') .OR. (CLAT(6:6).GT.'9')
      IF (.NOT.OLDNAM) THEN
         IF ((CLAT(3:3).LT.'0') .OR. (CLAT(3:3).GT.'9')) THEN
            READ (CLAT(4:6),1009) J0
         ELSE
            READ (CLAT(3:6),1008) J0
            END IF
         END IF
      DO 20 I = 1,NMOD
         IF (NITER(I).GT.0) WASOME = .TRUE.
 20      CONTINUE
C                                       Loop thru other fields.
 25   WASOFF = .FALSE.
      CLAT = MODCLS
      DO 40 I = 1,NMOD
         IF (FIRST) THEN
            CCDISK(I) = MODVOL
            CCVER(I) = VER
            NCLNG(I) = NITER(I)
            NSUBG(I) = MAX (1, BITER(I))
            CCCNO(I) = 1
            END IF
C                                       Set class.
         IF (OLDNAM) THEN
            I4T = I - 1
            IF (I.NE.1) CALL ZEHEX (I4T, 2, CLAT(5:6))
         ELSE
            JJ = I - 1 + J0
            IF (JJ.LE.999) THEN
               WRITE (CLAT(4:6),1010) JJ
            ELSE
               WRITE (CLAT(3:6),1011) JJ
               END IF
            END IF
         ITYPE = '  '
         CALL CATDIR ('SRCH', CCDISK(I), CCCNO(I), MODNAM, CLAT, MODSEQ,
     *       ITYPE, NLUSER, STAT, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, MODNAM, CLAT, MODSEQ, ITYPE,
     *      CCDISK(I), NLUSER
            IERR = 2
            IF (IRET.EQ.5) IERR = 3
            IF (IRET.EQ.3) IERR = 4
            GO TO 990
            END IF
C                                       Read catalog block
         STAT = 'REST'
         IF (FIRST) STAT = 'READ'
         CALL CATIO ('READ', CCDISK(I), CCCNO(I), CATCLN, STAT, BUFFER,
     *      IRET)
         IF (IRET.NE.0) THEN
            IERR = 4
            IF (IRET.GT.4) IERR = 2
            WRITE (MSGTXT,1015) IRET, CCDISK(I), CCCNO(I)
            GO TO 990
            END IF
         IF (FIRST) THEN
            IF (I.EQ.1) ISUV = ITYPE.EQ.'UV'
            IF ((ISUV) .AND. (ITYPE.NE.'UV')) THEN
               MSGTXT = 'MODEL FILES OF MIXED MA AND UV TYPES' //
     *            ' NOT ALLOWED'
               IERR = 5
               GO TO 990
               END IF
C                                       Do NOT mark in /CFILES/
C                                       force tiny => DFT for UV
            IF (ITYPE.EQ.'UV') THEN
               TMPFLD(1,I) = 24
               TMPFLD(2,I) = 24
               TMPFLD(3,I) = 24
               TMPFLD(4,I) = 24
C                                       Check FLDSIZ
            ELSE
               CALL IMGSIZ (CATCLN, CATCR, MX, MY, MXOFF, MYOFF, IRET)
               IF (IRET.GT.0) WASOFF = .TRUE.
               TMPFLD(1,I) = MX
               TMPFLD(2,I) = MY
               TMPFLD(3,I) = CATCLN(KINAX)
               TMPFLD(4,I) = CATCLN(KINAX+1)
               END IF
C                                       Get stokes' type for first
            IF (I.EQ.1) THEN
               CALL AXEFND (8, STOKES, KICTPN, CATCH(KHCTP), ISOFF,
     *            IRET)
               IF (CATCD(KDCRV+ISOFF).GT.0.0D0) THEN
                  ISTOKE = CATCD(KDCRV+ISOFF) + 0.01D0
               ELSE
                  ISTOKE = CATCD(KDCRV+ISOFF) - 0.01D0
                  END IF
               END IF
C                                       epoch
            IF (DEQUIN.NE.CATCR(KREPO)) THEN
               IERR = 5
               WRITE (MSGTXT,1016) I, CATCR(KREPO), DEQUIN
               CALL MSGWRT (8)
               MSGTXT = 'USE UVFIX ON THE UV DATA OR EPOSW ON THE IMAGE'
               GO TO 990
               END IF
            END IF
C                                       3D image?
         CHTEMP = 'RA'
         CALL AXEFND (2, CHTEMP, KICTPN, CATCH(KHCTP), IRAOFF, IERR)
         CHTEMP = 'DEC'
         CALL AXEFND (3, CHTEMP, KICTPN, CATCH(KHCTP), IDECOF, IERR)
         IF (FIRST) THEN
            IF (((ABS(CATCR(KRCRP+IRAOFF)-CATCLN(KINAX+IRAOFF)/2.0).LT.
     *         0.01) .AND. (ABS(CATCR(KRCRP+IDECOF)-1.0-
     *         CATCLN(KINAX+IDECOF)/2.0).LT.0.01)) .OR.
     *         (CATCLN(KIITY).EQ.2) .OR. (ISUV)) NCEN = NCEN + 1
            IF ((ABS(CATCD(KDCRV+IRAOFF)-RA).GT.
     *         0.01*ABS(CATCR(KRCIC+IRAOFF))) .OR.
     *         (ABS(CATCD(KDCRV+IDECOF)-DEC).GT.
     *         0.01*ABS(CATCR(KRCIC+IDECOF)))) NOFF = NOFF + 1
            END IF
C                                       rotation matrix in case
         IF (DOUMAT) THEN
            IF (.NOT.WASDIF) CALL RCOPY (9, UMATS(1,1,I), UMATN)
            CLNRA = CATCD(KDCRV+IRAOFF)
            CLNDEC = CATCD(KDCRV+IDECOF)
            CALL ROTFND (CATCR, MAPROT, IRET)
            IF (DO3DIM) THEN
               CALL PRJMAT (RA, DEC, UVROT, CLNRA, CLNDEC, MAPROT,
     *            UMATS(1,1,I), PMAT)
            ELSE
               CALL P2DMAT (RA, DEC, UVROT, CLNRA, CLNDEC, MAPROT,
     *            UMATS(1,1,I), PMAT)
               END IF
            IF (.NOT.WASDIF) THEN
               DO 35 K = 1,3
                  DO 30 J = 1,3
                     IF (ABS(UMATS(J,K,I)-UMATN(J,K)).GT.3.0E-6)
     *                  WASDIF = .TRUE.
 30                  CONTINUE
 35               CONTINUE
               END IF
            END IF
C                                       Check model type.
         IF ((MODEL.LT.2) .AND. (FIRST)) THEN
C                                       Get number of CC.
            JNREC = 1
            JLREC = 3
            NKEY = 0
            IF (MODEL.EQ.0) MSGSUP = 32000
            CALL TABINI ('READ', 'CC', CCDISK(I), CCCNO(I), CCVER(I),
     *         CATCLN, LUNC, NKEY, JNREC, JLREC, BUFFER, IBUFF, IRET)
            MSGSUP = MSGSAV
            IF (IRET.EQ.2) THEN
               IERR = 0
               NCLNG(I) = 0
               NITER(I) = 0
            ELSE IF (IRET.GT.0) THEN
               IERR = 4
               IF (IRET.EQ.2) IERR = 3
               IF (MODEL.EQ.0) IERR = 0
C                                       If problem, use image
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  CALL MSGWRT (6)
                  MSGTXT = '*** WARNING: OVERRIDING SPECIFIED' //
     *               ' COMPUTATION METHOD'
                  IF ((METHOD.NE.1) .AND. (METHOD.NE.0)) CALL MSGWRT (6)
                  END IF
               MODEL = 2
               METHOD = 1
C                                       Check number of components.
            ELSE
               I4T = IBUFF(5)
               IF (NCLNG(I).LT.0) NCLNG(I) = I4T
               IF ((.NOT.WASOME) .AND. (NCLNG(I).EQ.0)) NCLNG(I) = I4T
               IF (NCLNG(I).GT.I4T) NCLNG(I) = I4T
               IF (NCLNG(I).GE.NSUBG(I)) SUM2 = SUM2 +
     *            NCLNG(I) - NSUBG(I) + 1
C                                       Reset NITER to true value
               NITER(I) = NCLNG(I)
C                                       Close CLEAN components file.
               CALL TABIO ('CLOS', 1, I4T, BUFFER, IBUFF, IRET)
               END IF
            END IF
 40      CONTINUE
C                                       Check that there are Comps.
      IF (FIRST) THEN
         IF (SUM2.LE.0) THEN
            IF (MODEL.EQ.1) THEN
               MSGTXT = 'NO CLEAN COMPONENTS, WILL USE IMAGE'
               CALL MSGWRT (6)
               IERR = 3
               END IF
            MODEL = 2
            METHOD = 1
            END IF
         MODEL = MAX (1, MODEL)
C                                       Check/set 3D
         DO3DIM = .FALSE.
         IF (NOFF.EQ.0) THEN
            MSGTXT = 'SETGDS: imaging done with one tangent plane'
            IF ((MFIELD.GT.1) .AND. (ONCE.NE.1)) THEN
               CALL MSGWRT (2)
               MSGTXT = 'SETGDS: WARNING, THIS HAS CHANGED'
               IF (ONCE.NE.-1) CALL MSGWRT (6)
               END IF
            ONCE = 1
            IF (ISUV) DO3DIM = .TRUE.
         ELSE IF (NCEN.EQ.NMOD) THEN
            DO3DIM = .TRUE.
            MSGTXT = 'SETGDS: imaging done with reprojected tangent'
     *         // ' point(s)'
            IF (ONCE.NE.2) THEN
               CALL MSGWRT (4)
               MSGTXT = 'SETGDS: WARNING, THIS HAS CHANGED'
               IF (ONCE.NE.-1) CALL MSGWRT (6)
               END IF
            ONCE = 2
         ELSE IF (NCEN.EQ.0) THEN
            MSGTXT = 'SETGDS: imaging done with one OFFSET' //
     *         ' tangent plane'
            IF (ONCE.NE.3) THEN
               CALL MSGWRT (2)
               MSGTXT = 'SETGDS: WARNING, THIS HAS CHANGED'
               IF (ONCE.NE.-1) CALL MSGWRT (6)
               END IF
            ONCE = 3
         ELSE
            WRITE (MSGTXT,1050) NCEN, NOFF, NMOD
            CALL MSGWRT (7)
            ONCE = 0
            END IF
         IF ((DO3DIM) .AND. (WASOFF)) THEN
            MSGTXT = 'DO3DIM SET TRUE, BUT REF PIXELS NOT ALL INTEGER!'
            CALL MSGWRT (8)
            MSGTXT = 'LIKELY TO CAUSE REAL PROBLEMS IN MODELING'
            CALL MSGWRT (8)
            END IF
         IF ((DO3DIM.NEQV.IN3D) .AND. (DOUMAT)) THEN
            FIRST = .FALSE.
            GO TO 25
            END IF
         END IF
C                                       set FLDSZ if needed
      DO 50 I = 1,NMOD
         IF (MODEL.EQ.3) THEN
            IF (FLDSZ(1,I).LE.0) FLDSZ(1,I) = TMPFLD(1,I)
            IF (FLDSZ(2,I).LE.0) FLDSZ(2,I) = TMPFLD(2,I)
         ELSE
            IF (FLDSZ(1,I).LE.0) FLDSZ(1,I) = TMPFLD(3,I)
            IF (FLDSZ(2,I).LE.0) FLDSZ(2,I) = TMPFLD(4,I)
            END IF
 50      CONTINUE
C                                       check DFT needed?
         LFORCE = FORCED (NMOD, FLDSZ(1,1))
C                                       forced method
         IF (LFORCE) THEN
            IF (METHOD.EQ.1) THEN
               MSGTXT = 'IMAGE SIZES => DFT IS MUCH BETTER THAN GRIDDED'
     *            // ' CONTINUING ANYWAY'
               CALL MSGWRT (8)
            ELSE IF (METHOD.EQ.0) THEN
               METHOD = -1
               END IF
            END IF
C                                       Header keyword
      IF ((DOUMAT) .AND. (WASDIF)) THEN
         CALL CATKEY ('REED', UVDISK, UVCNO, 'MAXABSU ', 1, LOCS,
     *      VALUES, KEYTYP, BUFFER, I)
         IF ((I.EQ.0) .AND. (LOCS(1).GT.0)) THEN
            CALL RCOPY (1, VALUES(LOCS(1)), MAXBLN)
            MAXBLN = -ABS(MAXBLN)
         ELSE
            MAXBLN = -1.0
            END IF
         LOCS(1) = 1
         CALL RCOPY (1, MAXBLN, VALUES(1))
C                                       Record Max Baseline in header
         CALL CATKEY ('WRIT', UVDISK, UVCNO, 'MAXABSU', 1, LOCS,
     *      VALUES, 2, BUFFER, I)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,2X,A2,' DISK=',
     *   I3,' USID=',I4)
 1008 FORMAT (I4)
 1009 FORMAT (I3)
 1010 FORMAT (I3.3)
 1011 FORMAT (I4.4)
 1015 FORMAT ('ERROR',I3,' COPYING CATBLK DISK,CNO',I3,I6)
 1016 FORMAT ('SETGDS: IMAGE',I5,' EPOCH',F7.1,' UV EPOCH',F7.1)
 1020 FORMAT ('PROBLEM WITH CC FILE(ERROR=',I3,'), WILL USE THE IMAGE')
 1050 FORMAT ('SETGDS: CENTERED',I3,' OFFSET',I3,' IN',I3,' MODELS',
     *   ' 3DIMAG INCONSISTENT')
      END
