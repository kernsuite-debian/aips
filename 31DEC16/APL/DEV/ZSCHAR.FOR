      CHARACTER FUNCTION ZSCHAR (CNAME)
C-----------------------------------------------------------------------
C! Return special characters
C# Z Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1996
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
C Returns the special character denoted by CNAME or the closest
C equivalent if that character is not supported on this system.
C The possible values for CNAME are:
C
C     backslash
C     alert
C     form-feed
C     new-line
C     cr
C     tab
C     vt
C
C 'alert' is the character that causes the terminal to beep, 'cr'
C is the carriage return character, 'tab' is the horizontal tab
C character and 'vt' is the vertical tab character. The backslash
C character is regarded as special because FORTRAN compilers often
C have problems dealing with it under Unix.
C
C Inputs:
C    CNAME      C*(*)    The name of the character to be returned.
C                        This must be given in full and must be
C                        either upper or lower-case (but not a
C                        mixture of the two.
C
C Returned value
C    ZSCHAR     C*1      The character corresponding to CNAME if
C                        the value of CNAME appears in the list
C                        given above or the question mark
C                        character ('?') if not.
C
C This version is for machines using ASCII encoding and European
C variants.
C-----------------------------------------------------------------------
      CHARACTER CNAME*(*)
C-----------------------------------------------------------------------
      IF ((CNAME.EQ.'backslash').OR.(CNAME.EQ.'BACKSLASH')) THEN
         ZSCHAR = CHAR (92)
      ELSE IF ((CNAME.EQ.'alert').OR.(CNAME.EQ.'ALERT')) THEN
         ZSCHAR = CHAR (7)
      ELSE IF ((CNAME.EQ.'form-feed').OR.(CNAME.EQ.'FORM-FEED')) THEN
         ZSCHAR = CHAR (11)
      ELSE IF ((CNAME.EQ.'new-line').OR.(CNAME.EQ.'NEW-LINE')) THEN
         ZSCHAR = CHAR (10)
      ELSE IF ((CNAME.EQ.'cr').OR.(CNAME.EQ.'CR')) THEN
         ZSCHAR = CHAR (13)
      ELSE IF ((CNAME.EQ.'tab').OR.(CNAME.EQ.'TAB')) THEN
         ZSCHAR = CHAR (9)
      ELSE IF ((CNAME.EQ.'vt').OR.(CNAME.EQ.'VT')) THEN
         ZSCHAR = CHAR (11)
      ELSE
         ZSCHAR = '?'
         END IF
      RETURN
      END
