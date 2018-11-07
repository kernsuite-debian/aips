      SUBROUTINE SKPBLK (FITBLK, ICARD, FDVEC, TAPBLK, TBIND, IERR)
C-----------------------------------------------------------------------
C! find next non-blank card image in a FITS header, read tape if needed
C# FITS tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Routine to skip blank cards in a buffer.  Cards and buffer are FITS
C   type, i.e. 80 byte cards and 36 card buffer.  It will read more tape
C   records (and convert to local characters) as needed.
C   In/Out:
C      FITBLK   C*2880       FITS record (36 local character cards).
C      ICARD    I            Current card number (starts at 1).  Will
C                            be next non blank card on output.
C      FDVEC    I(*)         TAPIO control buffer
C      TAPBLK   I(*)         Tape I/O buffer
C      TBIND    I            Tape I/O buffer pointer.
C   Output:
C      IERR     I            0=OK, else TAPIO error
C-----------------------------------------------------------------------
      CHARACTER FITBLK*2880
      INTEGER   ICARD, IERR, TBIND, TAPBLK(*), FDVEC(*)
C
      INTEGER   IC, IS, IE
C-----------------------------------------------------------------------
      IERR = 0
C                                       loop on tape records
 10   CONTINUE
C                                       read new record
         IF ((ICARD.GT.0) .AND. (ICARD.LE.36)) GO TO 20
            CALL TAPIO ('READ', FDVEC, TAPBLK, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL ZC8CL (2880, 1, TAPBLK(TBIND), FITBLK)
            ICARD = 1
C                                       look for non-blank card
 20      IC = ICARD
         DO 50 ICARD = IC,36
            IS = 80 * (ICARD - 1) + 1
            IE = IS + 79
            IF (FITBLK(IS:IE).NE.' ') GO TO 999
 50         CONTINUE
         ICARD = 37
         GO TO 10
C
 999  RETURN
      END
