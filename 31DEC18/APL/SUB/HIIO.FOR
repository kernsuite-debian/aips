      SUBROUTINE HIIO (OP, HPTR, NREC, BUFFER, IERR)
C-----------------------------------------------------------------------
C! does IO and file expansion (if needed) on HI files
C# History
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2005, 2013
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
C   HIIO performs IO on history files including manual expand of file
C   size when required.
C   Inputs:
C      OP      C*4       'READ' or 'WRIT'
C      HPTR    I         pointer to entry in HITAB
C      NREC    I         record number (512-byte records)
C   In/Out:
C      BUFFER  I(256)    HI work buffer
C   Out:
C      IERR    I         as ZFI3
C   Common:
C      Can reset HITAB(HPTR + 3 and 6) = max size, current rec #
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   HPTR, NREC, BUFFER(256), IERR
C
      INTEGER   LUN, IND, NSIZ, NMAX, NEWREC, LSIZE, IER
      CHARACTER HNAME*48
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        get parms from HITAB
      LUN = HITAB (HPTR)
      IND = HITAB (HPTR+1)
      NSIZ = (HITAB(HPTR+3) - 1) / NHILPR + 1
      NMAX = (HITAB(HPTR+2) - 1) / NHILPR + 1
C                                        test 456 => EOF,BOM,EOM
      IERR = 5
      IF (NREC.LE.0) GO TO 900
      IERR = 6
      IF (NREC.GT.NSIZ+1) GO TO 900
      IF ((OP.EQ.'READ') .AND. (NREC.GT.NSIZ)) GO TO 900
C                                        Is IO justified by current
C                                        logical count (NMAX > NSIZ)
      IERR = 4
      IF (NREC.GT.NMAX) GO TO 900
C                                        expand
      IF (NREC.GT.NSIZ) THEN
         NEWREC = 69/NHILPR + 1
         CALL ZPHFIL ('HI', HITAB(HPTR+4), HITAB(HPTR+5), 1, HNAME,
     *      IERR)
         CALL ZEXPND (LUN, HITAB(HPTR+4), HNAME, NEWREC, IERR)
C                                       real error?
         IF (IERR.NE.0) THEN
            CALL ZEXIST (HITAB(HPTR+4), HNAME, LSIZE, IER)
            IF (IER.EQ.0) THEN
               IF (LSIZE.GT.NSIZ) THEN
                  MSGTXT = 'Odd - the HI file got bigger, ' //
     *               'guess it is okay'
                  CALL MSGWRT (7)
                  IERR = 0
                  NEWREC = LSIZE - NSIZ
                  END IF
               END IF
            END IF
         IF (IERR.NE.0) GO TO 910
         HITAB(HPTR+3) = HITAB(HPTR+3) + NEWREC * NHILPR
         END IF
C                                        do IO
 20   CALL ZFIO (OP, LUN, IND, NREC, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 920
      HITAB(HPTR+6) = NREC
      GO TO 999
C                                        inputs error
 900  WRITE (MSGTXT,1900) LUN, NREC, NSIZ, NMAX, IERR
      GO TO 990
C                                        expand error
 910  WRITE (MSGTXT,1910) LUN, NREC, IERR
      GO TO 990
C                                        IO error
 920  WRITE (MSGTXT,1910) LUN, NREC, IERR
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('HIIO LUN, NREC, NSIZ, NMAX',4I7,' ERR',I7)
 1910 FORMAT ('HIIO ON LUN, NREC',2I7,' EXPAND ERROR',I7)
 1920 FORMAT ('HIIO ON LUN, NREC',2I7,' IO ERROR',I7)
      END
