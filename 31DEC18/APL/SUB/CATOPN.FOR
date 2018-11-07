      SUBROUTINE CATOPN (IVOL, CIND, BUFF, CMAX, IERR)
C-----------------------------------------------------------------------
C! opens the catalog directory file and returns its size
C# Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004
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
C   CATOPN opens the catalog file and returns pointer to the FTAB entry
C   and the total number of slots in the catalog.  It updates the last
C   access date/time in the catalog header.  It will create the catalog
C   file (private catalog systems only) and initialize it if needed.
C   Inputs:
C      IVOL   I        disk volume containing catalog
C                      If < 0, don't update access time or create
C   Outputs:
C      CIND   I        pointer to catalog LUN in FTAB
C      BUFF   I(256)   Working buffer
C      CMAX   I        Number of slots in catalog
C      IERR   I        Error return 0 => ok
C                                   1 => can't open cat file
C                                   2 => input error
C                                   3 => I/O error catalog file
C                                   4 => create error
C                                   5 => CA file forbidden on this disk
C-----------------------------------------------------------------------
      INTEGER   IVOL, CIND, BUFF(256), CMAX, IERR
C
      CHARACTER CNAME*48
      INTEGER   CLUN, IER, NREC, NCMAX, OPNER, INERR, IOERR, CRERR,
     *   LVOL, NWPL, NLPR, NL, I, IREC, ISIZE, LSIZE
      REAL      DTIM
      LOGICAL   T, F, IAMOK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DDRC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA OPNER, INERR, IOERR, CRERR /1,2,3,4/
      DATA CLUN, NCMAX /15, 3/
      DATA DTIM /2.5/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NL = 0
C                                       Allowed?
      LVOL = ABS(IVOL)
      IF (.NOT.IAMOK(LVOL,'CA')) THEN
         IERR = 5
         WRITE (MSGTXT,1000) LVOL, NLUSER
         GO TO 995
         END IF
C                                       open file
 10   IERR = INERR
      CALL ZPHFIL ('CA', LVOL, 0, 0, CNAME, IER)
      IF (IER.NE.0) GO TO 999
      IERR = OPNER
      CALL ZOPEN (CLUN, CIND, LVOL, CNAME, F, T, T, IER)
      IF (IER.EQ.0) THEN
C                                       get size of catalog
         IERR = 0
         IREC = 1
         CALL ZFIO ('READ', CLUN, CIND, IREC, BUFF, IER)
         IF (IER.NE.0) GO TO 900
         CMAX = BUFF(NCMAX)
C                                       update the last access time
         IF ((IVOL.GT.0) .AND. (DSKLOK(IVOL).LE.0)) THEN
            CALL ZDATE (BUFF(10))
            CALL ZTIME (BUFF(13))
            CALL ZFIO ('WRIT', CLUN, CIND, IREC, BUFF, IER)
            IF (IER.NE.0) GO TO 900
            END IF
C                                       Catalog file not found
C                                       Create if private, requested
      ELSE IF ((IER.EQ.2) .AND. (IVOL.GT.0) .AND. (UCTSIZ.GT.0)) THEN
         IERR = 0
C                                       file size
         NWPL = 10
         NLPR = 256 / NWPL
         ISIZE = 2 + (UCTSIZ-1) / NLPR
         CALL ZCREAT (LVOL, CNAME, ISIZE, F, LSIZE, IER)
C                                       it got created somehow
         IF ((IER.NE.1) .OR. (NL.GT.0)) GO TO 110
            NL = NL + 1
            CALL ZDELAY (DTIM)
            GO TO 10
 110     IF (IER.EQ.0) GO TO 115
            IERR = CRERR
            IF (IER.EQ.5) IERR = 5
            WRITE (MSGTXT,1110) LVOL, IER
            GO TO 995
 115     CALL ZOPEN (CLUN, CIND, LVOL, CNAME, F, T, T, IER)
         IF (IER.EQ.0) GO TO 120
            IERR = OPNER
            WRITE (MSGTXT,1115) LVOL, IER
            GO TO 995
 120     WRITE (MSGTXT,1120) LVOL
         CALL MSGWRT (5)
         CALL FILL (256, 0, BUFF)
C                                       How many will it actually hold
         NREC = LSIZE
         CMAX = (NREC - 1) * NLPR
         IF (CMAX.GT.46655) CMAX = 46655
         BUFF(1) = LVOL
         BUFF(3) = CMAX
         CALL ZDATE (BUFF(4))
         CALL ZTIME (BUFF(7))
         CALL COPY (6, BUFF(4), BUFF(10))
         IREC = 1
         CALL ZFIO ('WRIT', CLUN, CIND, IREC, BUFF, IER)
         IF (IER.NE.0) GO TO 900
C                                       Directory records
         CALL FILL (20, 0, BUFF)
         DO 150 I = 1,256,NWPL
            BUFF(I) = -1
 150        CONTINUE
         DO 155 I = 2,NREC
            IREC = I
            CALL ZFIO ('WRIT', CLUN, CIND, IREC, BUFF, IER)
            IF (IER.NE.0) GO TO 900
 155        CONTINUE
         END IF
      GO TO 999
C                                       IO error
 900  WRITE (MSGTXT,1900) IER, LVOL, IREC
      CALL MSGWRT (7)
      CALL ZCLOSE (CLUN, CIND, IER)
      IERR = IOERR
      GO TO 999
C                                       Error write
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATOPN: ACCESS DENIED TO DISK',I3,' FOR USER',I5)
 1110 FORMAT ('CATOPN: CANNOT CREATE CATALOG ON DISK',I3,' IER',I5)
 1115 FORMAT ('CATOPN: CANNOT OPEN CATALOG ON DISK',I3,' IER',I5)
 1120 FORMAT ('CATOPN: had to create and initialize catalog on disk',I3)
 1900 FORMAT ('CATOPN: CATALOG IO ERROR',I5,' DISK',I3,' RECORD',I5)
      END
