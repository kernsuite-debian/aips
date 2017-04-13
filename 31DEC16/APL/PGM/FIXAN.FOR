      PROGRAM FIXAN
C-----------------------------------------------------------------------
C! Prints contents of ANtenna file
C# Calibration Utility VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2012, 2016
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
C   FIXAN prints the contents of an antenna 'AN' file on the line
C   printer.
C   Inputs:
C      AIPS ADVERB    PRGM NAME            Description.
C      USERID           ID          User id number for desired file.
C      INNAME           NAME        Name of desired uv data file.
C      INCLASS          CLASS       Class of desired uv data file.
C      INSEQ            SEQ         Seq. no. of desired file.
C      INDISK           DISK        Disk number of desired file.
C      INVER            VER         Version number of ext. file
C                                   0 => most recent
C      OPTYPE           OPTYPE      'CENTR' only known one
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      HOLLERITH XNAME(3), XCLASS(2), XOPTYP
      CHARACTER PRGM*6, NAME*12, CLASS*6, ATIME*8, ADATE*12, OPTYPE*4,
     *   UTYPE*2, STAT*4, HILINE*72, KEYWRD(3)*8
      INTEGER   SEQ, DISK, VER, IABUF(512), LUN, FIND, LUNA, CNO, IERR,
     *   DATE(3), IT(3), NPARM, IRET, ID, IANT, NANT, IROUND, NSUM,
     *   BUFFER(256), IABUF2(512), VER2, LUNB, I, HLUN, LOCS(3),
     *   KEYTYP(3)
      LOGICAL   RQUICK
      REAL      XSEQ, XDISK, XVER, OVER, ABUFF(512)
      DOUBLE PRECISION OLDC(3), NEWC(3), DX, DY, COSA, SINA, ARRLON
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INCOM/ XNAME, XCLASS, XSEQ, XDISK, XVER, OVER, XOPTYP
      EQUIVALENCE (IABUF, ABUFF)
      DATA PRGM /'FIXAN '/
      DATA LUN, LUNA, LUNB, HLUN /16, 29,30, 31/
      DATA KEYWRD /'ARRAYX','ARRAYY','ARRAYZ'/
      DATA LOCS, KEYTYP /1,3,5, 3*1/
C-----------------------------------------------------------------------
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                        Get parms.
      NPARM = 10
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         RQUICK = .FALSE.
         GO TO 990
         END IF
      IRET = 0
C                                        Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IRET = 8
C                                        Crunch parameters.
      ID = NLUSER
      SEQ = IROUND (XSEQ)
      DISK = IROUND (XDISK)
      VER = IROUND (XVER)
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      UTYPE = 'UV'
      STAT = 'WRIT'
      CALL MAPOPN (STAT, DISK, NAME, CLASS, SEQ, UTYPE, ID, LUN,
     *   FIND, CNO, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAME, CLASS, SEQ, 'UV', DISK, ID
         CALL MSGWRT (8)
         GO TO 990
         END IF
      CALL FNDEXT ('AN', CATBLK, I)
      IF ((VER.LE.0) .OR. (VER.GT.I)) VER = I
      VER2 = IROUND (OVER)
      IF (VER2.LE.0) VER2 = VER
C                                        Init AN file.
      CALL ANTINI ('READ', IABUF, DISK, CNO, VER, CATBLK, LUNA, IANRNO,
     *   ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL,
     *   ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) GO TO 980
      NANT = IABUF(5)
C                                       CENT operation
C                                       assumes stupid Earth surface
C                                       coords
      IF (OPTYPE.EQ.'WRNG') THEN
         IF ((ARRAYC(1).LE.1.D0) .AND. (ARRAYC(2).LE.1.D0) .AND.
     *      (ARRAYC(3).LE.1.D0)) THEN
            MSGTXT = 'WRNG REQUIRES EARTH SURFACE ARRAY CENTER'
            CALL MSGWRT (8)
            IRET = 10
            GO TO 970
            END IF
         OLDC(1) = ARRAYC(1)
         OLDC(2) = ARRAYC(2)
         OLDC(3) = ARRAYC(3)
         ARRAYC(1) = 0.0D0
         ARRAYC(2) = 0.0D0
         ARRAYC(3) = 0.0D0
         NEWC(1) = ARRAYC(1)
         NEWC(2) = ARRAYC(2)
         NEWC(3) = ARRAYC(3)
         CALL ANTINI ('WRIT', IABUF2, DISK, CNO, VER2, CATBLK, LUNB,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         IF (IRET.NE.0) GO TO 980
         IF ((NEWC(1).NE.ARRAYC(1)) .OR. (NEWC(2).NE.ARRAYC(2)) .OR.
     *      (NEWC(3).NE.ARRAYC(3))) THEN
            CALL TABKEY ('WRIT', KEYWRD, 3, IABUF2, LOCS, NEWC,
     *         KEYTYP, IRET)
            IF (IRET.NE.0) GO TO 970
            END IF
C                                       do the naive fix
         DO 20 IANT = 1,NANT
            IANRNO = IANT
            CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.GT.0) GO TO 970
            IF (IRET.EQ.0) THEN
               STAXYZ(1) = STAXYZ(1) + OLDC(1)
               STAXYZ(2) = STAXYZ(2) + OLDC(2)
               STAXYZ(3) = STAXYZ(3) + OLDC(3)
               IANRNO = IANT
               CALL TABAN ('WRIT', IABUF2, IANRNO, ANKOLS, ANNUMV,
     *            ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *            FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB,
     *            IRET)
               IF (IRET.GT.0) GO TO 970
               END IF
 20         CONTINUE
C                                       Correct surface -> centric
      ELSE IF (OPTYPE.EQ.'CENT') THEN
         IF ((ARRAYC(1).LE.1.D0) .AND. (ARRAYC(2).LE.1.D0) .AND.
     *      (ARRAYC(3).LE.1.D0)) THEN
            MSGTXT = 'CENT REQUIRES EARTH SURFACE ARRAY CENTER'
            CALL MSGWRT (8)
            IRET = 10
            GO TO 970
            END IF
         ARRLON = ATAN2 (ARRAYC(2), ARRAYC(1))
         SINA = SIN (ARRLON)
         COSA = COS (ARRLON)
         OLDC(1) = ARRAYC(1)
         OLDC(2) = ARRAYC(2)
         OLDC(3) = ARRAYC(3)
         ARRAYC(1) = 0.0D0
         ARRAYC(2) = 0.0D0
         ARRAYC(3) = 0.0D0
         NEWC(1) = ARRAYC(1)
         NEWC(2) = ARRAYC(2)
         NEWC(3) = ARRAYC(3)
         CALL ANTINI ('WRIT', IABUF2, DISK, CNO, VER2, CATBLK, LUNB,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         IF (IRET.NE.0) GO TO 980
         IF ((NEWC(1).NE.ARRAYC(1)) .OR. (NEWC(2).NE.ARRAYC(2)) .OR.
     *      (NEWC(3).NE.ARRAYC(3))) THEN
            CALL TABKEY ('WRIT', KEYWRD, 3, IABUF2, LOCS, NEWC,
     *         KEYTYP, IRET)
            IF (IRET.NE.0) GO TO 970
            END IF
C                                       do the correct fix
         DO 30 IANT = 1,NANT
            IANRNO = IANT
            CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.GT.0) GO TO 970
            IF (IRET.EQ.0) THEN
               DX = STAXYZ(1)*COSA - STAXYZ(2)*SINA
               DY = STAXYZ(2)*COSA + STAXYZ(1)*SINA
               STAXYZ(1) = OLDC(1) + DX
               STAXYZ(2) = OLDC(2) + DY
               STAXYZ(3) = OLDC(3) + STAXYZ(3)
               IANRNO = IANT
               CALL TABAN ('WRIT', IABUF2, IANRNO, ANKOLS, ANNUMV,
     *            ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *            FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB,
     *            IRET)
               IF (IRET.GT.0) GO TO 970
               END IF
 30         CONTINUE
C                                       assumes Earth centric ->
C                                       correct surface oriented
      ELSE IF (OPTYPE.EQ.'SURF') THEN
         IF ((ARRAYC(1).GT.1.D0) .OR. (ARRAYC(2).GT.1.D0) .OR.
     *      (ARRAYC(3).GT.1.D0)) THEN
            MSGTXT = 'SURF REQUIRES EARTH CENTRIC ARRAY CENTER'
            CALL MSGWRT (8)
            IRET = 10
            GO TO 970
            END IF
C                                       average
         NSUM = 0
         DO 50 IANT = 1,NANT
            IANRNO = IANT
            CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.GT.0) GO TO 970
            IF (IRET.EQ.0) THEN
               IF ((ABS(STAXYZ(1)).GT.1.0D0) .OR.
     *            (ABS(STAXYZ(2)).GT.1.0D0) .OR.
     *            (ABS(STAXYZ(3)).GT.1.0D0)) NSUM = NSUM + 1
               ARRAYC(1) = ARRAYC(1) + STAXYZ(1)
               ARRAYC(2) = ARRAYC(2) + STAXYZ(2)
               ARRAYC(3) = ARRAYC(3) + STAXYZ(3)
               END IF
 50         CONTINUE
         ARRAYC(1) = ARRAYC(1) / NSUM
         ARRAYC(2) = ARRAYC(2) / NSUM
         ARRAYC(3) = ARRAYC(3) / NSUM
         IF ((ANAME.EQ.'VLA') .OR. (ANAME.EQ.'EVLA')) THEN
            ARRAYC(1) = -1601185.365D0
            ARRAYC(2) =  -5041977.547D0
            ARRAYC(3) =  3554875.87D0
            END IF
         ARRLON = ATAN2 (ARRAYC(2), ARRAYC(1))
         SINA = SIN (ARRLON)
         COSA = COS (ARRLON)
         NEWC(1) = ARRAYC(1)
         NEWC(2) = ARRAYC(2)
         NEWC(3) = ARRAYC(3)
         CALL ANTINI ('WRIT', IABUF2, DISK, CNO, VER2, CATBLK, LUNB,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         IF (IRET.NE.0) GO TO 980
         IF ((NEWC(1).NE.ARRAYC(1)) .OR. (NEWC(2).NE.ARRAYC(2)) .OR.
     *      (NEWC(3).NE.ARRAYC(3))) THEN
            CALL TABKEY ('WRIT', KEYWRD, 3, IABUF2, LOCS, NEWC,
     *         KEYTYP, IRET)
            IF (IRET.NE.0) GO TO 970
            END IF
         DO 60 IANT = 1,NANT
            IANRNO = IANT
            CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.GT.0) GO TO 970
            DX = STAXYZ(1) - NEWC(1)
            DY = STAXYZ(2) - NEWC(2)
            STAXYZ(1) = DX * COSA + DY * SINA
            STAXYZ(2) = DY * COSA - DX * SINA
            STAXYZ(3) = STAXYZ(3) - NEWC(3)
            IANRNO = IANT
            CALL TABAN ('WRIT', IABUF2, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.GT.0) GO TO 970
 60         CONTINUE
         END IF
C                                       should be HI
      CALL ZTIME (IT)
      CALL DATEST (RDATE, DATE)
      CALL TIMDAT (IT, DATE, ATIME, ADATE)
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, DISK, CNO, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      WRITE (HILINE,1060) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (HLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      WRITE (HILINE,1061) TSKNAM, VER
      CALL HIADD (HLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      WRITE (HILINE,1062) TSKNAM, VER2
      CALL HIADD (HLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      IF (OPTYPE.EQ.'WRNG') THEN
         WRITE (HILINE,1066) TSKNAM
      ELSE IF (OPTYPE.EQ.'CENT') THEN
         WRITE (HILINE,1067) TSKNAM
      ELSE
         WRITE (HILINE,1068) TSKNAM
         END IF
      CALL HIADD (HLUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL HICLOS (HLUN, .TRUE., BUFFER, IERR)
C
 970  CALL TABAN ('CLOS', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME, STAXYZ,
     *   ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *   POLCA, POLTYB, POLAB, POLCB, IERR)
      CALL TABAN ('CLOS', IABUF2, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *   STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA,
     *   POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
 980  CALL MAPCLS ('WRIT', DISK, CNO, LUN, FIND, CATBLK, .TRUE.,
     *   BUFFER, IERR)
C
 990  CALL DIETSK (IRET, RQUICK, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   ' DISK=',I3,' USER=',I5)
 1060 FORMAT (A6,'RELEASE =''',A7,'''  /********* Start ',
     *   A12,A8)
 1061 FORMAT (A6,'INVERS =',I6,'  / input AN version number')
 1062 FORMAT (A6,'OUTVERS=',I6,'  / output AN version number')
 1066 FORMAT (A6,'OPTYPE=''WRNG''  / correct erroneous Earth',
     *   ' surface to Earth centric')
 1067 FORMAT (A6,'OPTYPE=''CENT''  / change correct Earth',
     *   ' surface to Earth centric')
 1068 FORMAT (A6,'OPTYPE=''SURF''  / change Earth centric',
     *   ' to correct Earth surface')
      END

