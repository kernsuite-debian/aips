      SUBROUTINE ZPATH (LLEN, LOGFIL, FLEN, FULNAM, FACT, IERR)
C-----------------------------------------------------------------------
C! convert a file name 'Logical:file' to full path name
C# Z2 IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   Translate a file specifcation of the form "logical:file" to a
C   full path name.
C   Inputs:
C      LLEN     I          Length of "logfil" (1-relative)
C      LOGFIL   H*(*)      File specification (may contain a valid,
C                          colon terminated logical variable)
C      FLEN     I          Length of "fulnam" (1-relative)
C   Output:
C      FULNAM   H*(flen)   Full path name (blank filled)
C      FACT     I          Position of last non-blank in "fulnam"
C                          (1-relative)
C      IERR     I          Error return code: 0 => no error
C                              1 => improper combination of inputs
C                              2 => no translation for "logical"
C                              3 => "fulnam" too short
C                              4 => logical name required
C   VMS version - does not require a logical
C   When filling in the default area ZPATH does get the default disk
C   directory, but does not get the disk itself (or a logical name for
C   the disk itself) at present.  We seem to be overlooking a system
C   service somewhere.  (DCL command F$ENVIRONMENT("DEFAULT") does what
C   we want - where is the corresponding SYS$...?)
C-----------------------------------------------------------------------
      INTEGER   LLEN, FLEN, FACT, IERR
      HOLLERITH LOGFIL(*), FULNAM(*)
C
      CHARACTER ENVAR*256, XLATED*256, FILENA*256, PATHNA*256,
         LLOGF*256, LFULN*256
      INTEGER   ELEN, XLEN, XLNB, JERR, I, J, IC, IA, IB, IP, ITRIM,
     *   SYS$SETDDIR, ILEN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE '($RMSDEF)'
C-----------------------------------------------------------------------
      IERR = 0
      CALL H2CHR (LLEN, 1, LOGFIL, LLOGF)
      ILEN = ITRIM (LLOGF(:LLEN))
C                                       get any logical part of name
      IC = INDEX (LLOGF, ':')
      IA = INDEX (LLOGF, ']')
      IB = INDEX (LLOGF, ']')
      IP = INDEX (LLOGF(IB+1:), '.')
C                                       No logical/device :
      IF (IC.LE.0) THEN
C                                       Just a file name
         IF ((IA.LE.0) .AND. (IB.LE.0)) THEN
C                                       get default dirrectory
C                                       Do not know how to get disk!!
            JERR = SYS$SETDDIR ( , ELEN, ENVAR)
            IF (JERR.NE.RMS$_NORMAL) THEN
               IERR = 2
               WRITE (MSGTXT,1000) JERR
               CALL MSGWRT (6)
            ELSE
               LFULN = ENVAR(:ELEN) // LLOGF(:ILEN)
               FACT = ELEN + ILEN
               END IF
C                                       directory given
         ELSE IF ((IB.GT.IA) .AND. (IA.GT.0)) THEN
            LFULN = LLOGF(:ILEN)
            FACT = ILEN
C                                       bad format
         ELSE
            IERR = 1
            END IF
C                                       DECNET name (::)
      ELSE IF (LLOGF(IC+1:IC+1).EQ.':') THEN
C                                       name must be spelled out
         IF ((IB.GT.IA) .AND. (IA.GT.0)) THEN
            LFULN = LLOGF(:ILEN)
            FACT = ILEN
C                                       error
         ELSE
            IERR = 1
            END IF
C                                       Logical or device present
      ELSE
         ELEN = IC - 1
         ENVAR = LLOGF(:ELEN)
         XLEN = LEN (XLATED)
         CALL ZTRLOG (ELEN, ENVAR, XLEN, XLATED, XLNB, JERR)
C                                       misc error
         IF (JERR.GT.1) THEN
            IERR = 1
C                                       no translation
         ELSE IF (JERR.EQ.1) THEN
C                                       name must be spelled out
            IF ((IB.GT.IA) .AND. (IA.GT.0)) THEN
               LFULN = LLOGF(:ILEN)
               FACT = ILEN
C                                       error
            ELSE
               IERR = 1
               END IF
C                                       good translation
         ELSE
            I = IC
            IF ((XLATED(XLNB:XLNB).EQ.':') .AND.
     *         (LLOGF(IC:IC).EQ.':')) I = IC + 1
            IF ((XLATED(XLNB:XLNB).EQ.']') .AND.
     *         (LLOGF(IC:IC).EQ.':')) I = IC + 1
            IF ((XLATED(XLNB:XLNB).EQ.']') .AND.
     *         (LLOGF(IC:IC+1).EQ.':[')) THEN
               I = IC + 2
               XLNB = XLNB - 1
               END IF
            LFULN = XLATED(:XLNB) // LLOGF(I:ILEN)
            FACT = XLNB + ILEN - I + 1
            END IF
         END IF
C                                       Need to add a period ?
C                                       check length too
      IF (IERR.LE.0) THEN
         IF (IP.LE.0) FACT = FACT + 1
         IF (FACT.GT.FLEN) THEN
            IERR = 3
         ELSE
            IF (IP.LE.0) LFULN(FACT:FACT) = '.'
            END IF
         END IF
      CALL CHR2H (FLEN, LFULN, 1, FULNAM)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZPATH: ERROR ',Z8,' TRYING TO GET DEFAULT DIRECTORY')
      END
