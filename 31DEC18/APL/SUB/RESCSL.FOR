      SUBROUTINE  RESCSL (DISK, CNO, SCALE, OFFSET, LUN, BUFFER, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C! Rescale flux-like data in any SLice files.
C# EXT-util
C-----------------------------------------------------------------------
C   This routine applies a scaling and offset to the amplitude like data
C   in any SLice extention files associated with a cataloged image.
C   No modification is made to the catalog status.
C   Inputs:
C      DISK     I        Disk number of the file.
C      CNO      I        Catalog slot number.
C      SCALE    D        Scale factor, new = SCALE * old + OFFSET
C      OFFSET   D        Offset
C      LUN      I        Lun to use for I/O
C   Input in common /MAPHDR/ (include DCAT.INC):
C      CATBLK   I        Catalog header record
C   Output:
C      BUFFER   I(512)   I/O buffer for EXTINI
C      IERR     I        0 => OK, 5 => EXTINI error, 6 => ZFIO error,
C                        7 => EXTIO close error.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, LUN, BUFFER(*), IERR
      DOUBLE PRECISION SCALE, OFFSET
C
      INTEGER   NOSL, SLVER, LOOP, JERR, LREC, NREC, RECNO, FIND,
     *   NOSLRC, I, J, NGAUS, RECI(256)
      LOGICAL   TABLE, EXIST, FITASC, OPEN
      REAL      RECR(256)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECI, RECR)
C-----------------------------------------------------------------------
      IERR = 0
C                                       Any SL tables?
      CALL FNDEXT ('SL', CATBLK, NOSL)
      IF (NOSL.LE.0) GO TO 999
C                                       Rescale SL tables
      LREC = 256
      NREC = 1
      OPEN = .FALSE.
      DO 500 SLVER = 1,NOSL
C                                       Table extant?
         CALL ISTAB ('SL', DISK, CNO, SLVER, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, JERR)
         IF (.NOT.EXIST .OR. (JERR.NE.0)) GO TO 500
C                                       Open table
         CALL EXTINI ('WRIT', 'SL', DISK, CNO, SLVER, CATBLK, LUN, FIND,
     *      LREC, NREC, BUFFER, JERR)
         IF (JERR.NE.0) THEN
            IERR = 5
            WRITE (MSGTXT, 1000) JERR
            GO TO 990
            END IF
         OPEN = .TRUE.
C                                       Do max-min in second record
         RECNO = 2
         CALL ZFIO ('READ', LUN, FIND, RECNO, RECR, JERR)
         IF (JERR.NE.0) THEN
            IERR = 6
            WRITE (MSGTXT, 1010) JERR, 'READ'
            GO TO 990
            END IF
C                                       Max
         RECR(34) = RECR(34) * SCALE + OFFSET
C                                       Min
         RECR(35) = RECR(35) * SCALE + OFFSET
C                                       Rewrite record
         CALL ZFIO ('WRIT', LUN, FIND, RECNO, RECR, JERR)
         IF (JERR.NE.0) THEN
            IERR = 6
            WRITE (MSGTXT, 1010) JERR, 'WRIT'
            GO TO 990
            END IF
C                                       Slice data
         NOSLRC = (BUFFER(57) / 256.0) + 0.999
         DO 100 LOOP = 1,NOSLRC
            RECNO = 2 + LOOP
            CALL ZFIO ('READ', LUN, FIND, RECNO, RECR, JERR)
            IF (JERR.NE.0) THEN
               IERR = 6
               WRITE (MSGTXT, 1010) JERR, 'READ'
               GO TO 990
               END IF
C                                       Rescale record
            DO 50 I = 1,256
               IF (RECR(I).NE.FBLANK) RECR(I) = RECR(I) * SCALE + OFFSET
 50            CONTINUE
C                                       Rewrite record
            CALL ZFIO ('WRIT', LUN, FIND, RECNO, RECR, JERR)
            IF (JERR.NE.0) THEN
               IERR = 6
               WRITE (MSGTXT, 1010) JERR, 'WRIT'
               GO TO 990
               END IF
 100        CONTINUE
C                                       Slice models
         NOSLRC = BUFFER(58)
         IF (NOSLRC.LE.0) GO TO 400
         DO 200 LOOP = 1,NOSLRC
            RECNO = LOOP + BUFFER(59) - 1
            CALL ZFIO ('READ', LUN, FIND, RECNO, RECR, JERR)
            IF (JERR.NE.0) THEN
               IERR = 6
               WRITE (MSGTXT, 1010) JERR, 'READ'
               GO TO 990
               END IF
C                                       Rescale model amplitudes
            NGAUS = MIN (RECI(27), 4)
            DO 150 I = 1,NGAUS
               J = (I-1) * 4 + 1
               RECR(J) = RECR(J) * SCALE + OFFSET
               RECR(J+12) = RECR(J+12) * SCALE + OFFSET
 150           CONTINUE
C                                       Rewrite record
            CALL ZFIO ('WRIT', LUN, FIND, RECNO, RECR, JERR)
            IF (JERR.NE.0) THEN
               IERR = 6
               WRITE (MSGTXT, 1010) JERR, 'WRIT'
               GO TO 990
               END IF
 200        CONTINUE
C                                       Close file
 400     CALL EXTIO ('CLOS', LUN, FIND, RECNO, RECI, BUFFER, JERR)
         IF (JERR.NE.0) THEN
            IERR = 7
            WRITE (MSGTXT, 1200) JERR
            GO TO 990
            END IF
         OPEN = .FALSE.
 500     CONTINUE
C                                       Error
 990  CALL MSGWRT (6)
C                                       Close SL file if necessary
      IF (OPEN) CALL ZCLOSE (LUN, FIND, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RESCSL: EXTINI ERROR ',I3,' RESCALING SL FILE')
 1010 FORMAT ('RESCSL: ZFIO ERROR ',I3, 1X, A4, 'ING SL FILE')
 1200 FORMAT ('RESCSL: EXTIO ERROR ',I3, ' CLOSING SL FILE')
      END
