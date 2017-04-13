      SUBROUTINE ZFULLN (UNAME, DEFLOG, SUBR, FNAME, IERR)
C-----------------------------------------------------------------------
C! convert file name to full pathname with no logicals
C# IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2004
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
C   Convert file name from user specified name with or without a logical
C   to a full path and file name with the logical translated.
C   Inputs:
C      UNAME    C*(*)     User specified name - first a logical name,
C                         then a colon, then a file name (with or
C                         without a "." extension).  Logical optional.
C                         ' ' => create temporary file (DEFLOG not ' ')
C      DEFLOG   C*(*)     Default logical: ' ' => no default
C                         '-1' => UNAME must provide a logical
C      SUBR     C*6       Subroutine name to be used in creating a
C                         "temporary" file name.  ' ' => no temporary
C   Output:
C      FNAME    C*(*)     Full file name
C      IERR     I         Error code: 0 => okay
C                            1 => improper combination of inputs
C                            2 => no translation for logical
C                            3 => FNAME too short
C                            4 => logical required, not provided
C   Generic version - may not need to make any machine-specific versions
C   unless there is something really special/odd about logical names.
C-----------------------------------------------------------------------
      CHARACTER UNAME*(*), DEFLOG*(*), SUBR*(*), FNAME*(*)
      INTEGER   IERR
C
      INTEGER   ULEN, DLEN, SLEN, FLEN, ITRIM, ICOL, LLEN, XLEN, FACT
      CHARACTER LNAME*256, FILEN*256, LSUBR*6
      HOLLERITH HFILEN(64), HFNAME(64)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       lengths
      ULEN = ITRIM (UNAME)
      DLEN = ITRIM (DEFLOG)
      SLEN = ITRIM (SUBR)
      FLEN = LEN (FNAME)
      CALL CHR2H (256, ' ', 1, HFNAME)
      LSUBR = SUBR
      IF (LSUBR.EQ.' ') LSUBR = 'ZFULLN'
C                                       Check parms
      IERR = 1
      IF ((ULEN.EQ.0) .AND. ((DLEN.EQ.0) .OR. (SLEN.EQ.0))) GO TO 999
      ICOL = 0
C                                       absolute path
      IF ((ULEN.GT.0) .AND. (UNAME(:1).EQ.'/')) THEN
         LNAME = ' '
C                                       relative path
      ELSE
         IF (ULEN.GT.0) ICOL = INDEX (UNAME, ':')
         IF ((ICOL.LE.0) .AND. (DEFLOG.EQ.'-1')) GO TO 990
C                                       get logical
         IF (ICOL.GT.0) THEN
            LNAME = UNAME(1:ICOL-1)
         ELSE
            LNAME = DEFLOG
            END IF
         END IF
      LLEN = ITRIM (LNAME)
C
      IF (LLEN.LE.0) THEN
         FILEN = UNAME(ICOL+1:)
         XLEN = ULEN - ICOL
         CALL CHR2H (256, FILEN, 1, HFILEN)
         CALL ZPATH (XLEN, HFILEN, FLEN, HFNAME, FACT, IERR)
         LLEN = 1
      ELSE IF (ULEN.GT.0) THEN
         FILEN = LNAME(1:LLEN) // ':' // UNAME(ICOL+1:)
         XLEN = LLEN + 1 + ULEN - ICOL
         CALL CHR2H (256, FILEN, 1, HFILEN)
         CALL ZPATH (XLEN, HFILEN, FLEN, HFNAME, FACT, IERR)
      ELSE
         FILEN = LNAME(1:LLEN) // ':' // SUBR(1:SLEN) // '.XXXXXX'
         XLEN = LLEN + 1 + SLEN + 7
         CALL CHR2H (256, FILEN, 1, HFILEN)
         CALL ZPATH (XLEN, HFILEN, FLEN, HFNAME, FACT, IERR)
         IF (IERR.EQ.0) CALL ZMKTMP (FLEN, HFNAME, IERR)
         END IF
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) LSUBR, IERR, LNAME(1:MIN(LLEN,34))
         IF (IERR.EQ.2) WRITE (MSGTXT,1002) LSUBR, LNAME(1:MIN(LLEN,42))
         IF (IERR.EQ.3) WRITE (MSGTXT,1003) LSUBR, LNAME(1:MIN(LLEN,42))
         IF (IERR.EQ.4) WRITE (MSGTXT,1004) LSUBR, UNAME(1:MIN(ULEN,40))
         CALL MSGWRT (7)
      ELSE
         CALL H2CHR (FLEN, 1, HFNAME, FNAME)
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1004) LSUBR, UNAME(1:MIN(ULEN,40))
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,': ERROR',I3,' BUILDING FILE NAME, LNAME=''',A,'''')
 1002 FORMAT (A,': NO TRANSLATION FOR LNAME=''',A,'''')
 1003 FORMAT (A,': FILE NAME TOO LONG, LNAME=''',A,'''')
 1004 FORMAT (A,': LOGICAL NAME REQUIRED, FILE=''',A,'''')
      END
