      SUBROUTINE ALSINI (ALIST, SIZE, NDIM, NAME, IERR)
C-----------------------------------------------------------------------
C! Initialize Associative list (ALIST)
C# Utility 
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
C   Public function
C   Initialize ALIST.
C   An associative list (ALIST) lis a list of labeled values.  The
C   values may be of double precision, real, cahracter, integer or
C   logical data types.  A value can be a multi-dimensional array
C   (although for characters the first dimension is the length of the
C   string).  There is a minimum dimensionality of 1 and a maximum of
C   100. 
C      Arbitrary entries can be saved or stored subject to the
C   limitation of the size of the ALIST. All ALISTs are independent and
C   several may be used at the same time.  Numerous checks are made for
C   internal consistency. 
C      Direct references to the contents of an active ALIST are not
C   needed and SHOULD NOT BE MADE.  The public interface should be
C   used for all access.
C      Note the AIPS datatypes are defined as parameters in the INCLUDE
C   'INCS:PAOOF.INC': OOADP = double precision, OOARE = real, 
C   OOACAR = character, OOAINT = integer, and OOALOG = logical.
C
C   An ALIST has the following structure:
C   ALIST(ALSIZE) = number of words
C   ALIST(ALSNAM) = Hollerith name (32 char)
C   ALIST(ALSBEG) = First word of linked list.
C   ALIST(ALSEND) = Next free word in ALIST.
C   ALIST(ALSCHK) = Validity check word
C   ALIST(ALSNDM) = number of dimensions
C   ALIST(ALSHED) = number of words of list header
C   ALIST(ALSDIM) = offset of start of data.      
C   ALIST(ALIST(ALSBEG)-end) = Linked list with entries in the following
C      form: 
C          Word  Type        Contents
C        ------  ----  --------------------------------------------
C        ALSNXT    I    Address of next cell, .LE. 0 => last entry.
C        ALSKWD    H*16 Keyword as HOLLERITH
C        ALSTYP    I    Type: 1=D, 2=R, 3=C, 4=I, 5=L
C        ALSDIM    I    First dimension (length of string for char),
C                       higher dimensions follow
C        ALSDAT    ?    Data, characters as Hollerith
C           ...         As many words as necessary for the data.
C
C   Module parameters:
C     ALSIZE   PI      ALIST index of size
C     ALSNAM   PI      ALIST index of name
C     ALSBEG   PI      ALIST index of first
C     ALSEND   PI      ALIST index of next free word
C     ALSCHK   PI      ALIST index of validity check
C     ALSCVL   PI      ALIST validity check value
C     ALSNDM   PI      ALIST index of dimensionality  
C     ALSHED   PI      ALIST index of length of header
C     ALSDAT   PI      ALIST index of offset to start of data.
C     ALSNXT   PI      ALIST offset for next cell pointer.
C     ALSKWD   PI      ALIST offset for keyword
C     ALSTYP   PI      ALIST offset for data type
C     ALSDIM   PI      ALIST offset for 1st dimension
C
C   Public functions:
C      ALSINI (alist, size, ndim, name, ierr)
C         Initialize an alist
C      ALSGET (alist, keywrd, type, dim, value, valuec, ierr)
C         Fetch the value (array) for a specified keyword.
C         Recognizes special keyword 'LISTNAME' as the name of the ALIST
C         Recognizes special keyword 'GET SIZE' as the size of the ALIST
C      ALSPUT (alist, keywrd, type, dim, value, valuec, ierr)
C         Store the value (array) for a specified keyword.
C         Recognizes special keyword 'LISTNAME' as the name of the ALIST
C      ALSASK (alist, keywrd, type, dim, ierr)
C         Inquire about an entry.
C      ALSADD (alist, keywrd, type, dim, ierr)
C         Add an entry but store no data.
C      ALSDEL (alist, keywrd, ierr)
C         Remove a keyword entry from an ALIST and compress ALIST.
C      ALSDMP (alist)
C         Dump the contents of an ALIST to MSGWRT; intended for
C         debugging purposes.
C
C   Private functions:
C      ALSKEY (alist, keywrd, keypnt, ierr)
C         Lookup keyword in ALIST called by OBGET and OBPUT.
C      ALSC2H (string, ndim, dim, holl)
C         Convert a string (possibly an array) to hollerith
C      ALSH2C (holl, ndim, dim, string)
C         Convert a hollerith (possibly an array) to a string.
C
C   Inputs:
C      ALIST  I(*)  ALIST array
C      SIZE   I     Number of words in ALIST
C      NDIM   I     Max. dimensionality of any entry (min 1., max 100)
C      NAME   C*32  Name of ALIST
C   Outputs:
C      IERR   I  Return error code 0=>OK else failed
C-----------------------------------------------------------------------
      INTEGER   ALIST(*), SIZE, NDIM, IERR
      CHARACTER NAME*32
C
      INCLUDE 'INCS:PALIST.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Save size
      ALIST(ALSIZE) = SIZE
      ALIST(ALSNDM) = MAX (1, MIN (100,NDIM))
C                                       Save name
      CALL CHR2H (32, NAME, 1, ALIST(ALSNAM))
C                                       Initialize pointers
      ALIST(ALSBEG) = ALSDAT + 10
      ALIST(ALSEND) = ALIST(ALSBEG)
C                                       Length of header
      ALIST(ALSHED) = ALSDIM + ALIST(ALSNDM)
C                                       Offset to beginning of data
      ALIST(ALSDAT) = ALIST(ALSHED)
C                                       Initialize empty list
      ALIST(ALIST(ALSEND)) = -1
C                                       Save validity check
      ALIST(ALSCHK) = ALSCVL
C
 999  RETURN
      END
