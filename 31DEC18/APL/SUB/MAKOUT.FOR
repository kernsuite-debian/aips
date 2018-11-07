      SUBROUTINE MAKOUT (INN, INCL, INS, DEFCLS, OUTN, OUTCL, OUTS)
C-----------------------------------------------------------------------
C! convert input and output names to actual output names in standard way
C# Catalog
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
C   MAKOUT applies the wild card standards to complete the preparation
C   of the output file name parameters. Namely:
C   OUTS  <=  -1         becomes  OUTS = INSEQ
C   OUTN  =  ' '         becomes  OUTN = INN
C            'yy*zz '    becomes  OUTN = INN   with first n characters
C                        replaced by yy and last m chars with zz - if
C                        yy or zz contain ?'s don't replace those char
C                        positions
C   OUTCL =  '   '       becomes  OUTCL= DEFCLS
C            'yy*zz '    becomes  OUTCL= DEFCLS with same as OUTN
C           If the 1st character of OUTCL is a '_' then the default
C           is replaced with INCL and the remaining 5 characters of
C           OUTCL are used as normal.
C   Inputs:
C      INN     C*12     Input file name
C      INCL    C*6      Input file class
C      INS     I        Input file sequence number
C      DEFCLS  C*6      Default output file class 6 packed chars
C                       if = ' ', use task name
C   In/Out:
C      OUTN    C*12     User-supplied OUTNAME adverb
C      OUTCL   C*6      User-supplied OUTCLASS adverb
C      OUTS    I        User-supplied OUTSEQ adverb in integer
C   NOTE: the actual Input file name parameters must be supplied, not
C   the user adverbs (which can themselves contain wild cards, pure
C   blank fields, zeros, and the like.
C-----------------------------------------------------------------------
      CHARACTER INN*(*), INCL*(*), DEFCLS*(*), OUTN*(*), OUTCL*(*)
      INTEGER   INS, OUTS
C
      CHARACTER XXNAM*12, XXCLS*6, CTEMP*1
      INTEGER   NP, OP, I, IP, JTRIM
      LOGICAL   GOTAST
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Sequence number
      IF (OUTS.LE.-1) OUTS = INS
C                                       Output name
      XXNAM = INN
      NP = JTRIM (OUTN)
      IF (NP.LE.0) GO TO 40
         GOTAST = .FALSE.
         OP = 1
         DO 30 I = 1,NP
            CTEMP = OUTN(I:I)
            IF (CTEMP.NE.'?') THEN
               IF (CTEMP.NE.'*') THEN
                  XXNAM(OP:OP) = CTEMP
               ELSE IF (.NOT.GOTAST) THEN
                  OP = 12 + I - NP
                  GOTAST = .TRUE.
                  END IF
               END IF
            OP = OP + 1
 30         CONTINUE
         IF (GOTAST) GO TO 40
            NP = NP + 1
            IF (NP.LE.12) XXNAM(NP:) = ' '
 40   OUTN = XXNAM
C                                       Output class
      XXCLS = DEFCLS
      IF (XXCLS.EQ.' ') XXCLS = TSKNAM
      NP = JTRIM (OUTCL)
      IF (NP.LE.0) GO TO 80
         GOTAST = .FALSE.
         OP = 1
         IP = 1
         IF ((DEFCLS.NE.' ') .OR. (INCL.EQ.' ') .OR.
     *      (OUTCL(1:1).NE.'_')) GO TO 45
            IP = 2
            XXCLS = INCL
            IF (NP.EQ.1) GO TO 80
 45      DO 70 I = IP,NP
            CTEMP = OUTCL(I:I)
            IF (CTEMP.NE.'?') THEN
C                                       real char in OUTCL
               IF (CTEMP.NE.'*') THEN
                  XXCLS(OP:OP) = CTEMP
C                                       * in OUTCL
               ELSE IF (.NOT.GOTAST) THEN
                  OP = 6 + I - NP
                  GOTAST = .TRUE.
                  END IF
               END IF
            OP = OP + 1
 70         CONTINUE
         IF (GOTAST) GO TO 80
            NP = NP + 2 - IP
            IF (NP.LT.7) XXCLS(NP:) = ' '
 80   OUTCL = XXCLS
C
 999  RETURN
      END
