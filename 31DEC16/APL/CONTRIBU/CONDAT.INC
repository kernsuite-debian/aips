C               *****  CONDAT.DCL  *****    JANUARY 29, 1980
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
        DATA ISSUC/1/                   ! SUCCESSFUL COMPLETION FLAG
        DATA IENSF/-26/                 ! NO SUCH FILE FLAG VALUE
        DATA IECLO/-38/                 ! FILE NOT PROPERLY CLOSED FLAG
        DATA IEINS/-2/                  ! TASK NOT INSTALLED FLAG VALUE
        DATA PI/3.14159265/             ! PI
        DATA DPI/3.141592654D0/         ! DOUBLE PRECISION PI
        DATA CELSIZ/0,1,2,4/            ! NUMBER OF BYTES IN PIXEL CELL
        DATA DI512/512,0/               ! DOUBLE WORD 512
        DATA INTDIM,INTMED,INTBRT/2,4,8/ ! INTENSITIES FOR VT-11
        DATA OVLFMT,PIXFMT,I16FMT,REAFMT/1,2,3,4/ ! FORMATS OF MAP DATA
C                                                   ELEMENTS
        DATA QQQIND/"177777,"177777/    ! INDEFINITE VALUES - 8&32-BIT
        DATA I16IND/"100000/            ! INDEFINITE VALUE - 16-BIT
        DATA MJAD0/-678576.0/           ! MJAD FOR YEAR 0, DAY 0
        DATA ARSRAD/206264.8062D0/      ! ARC SECONDS IN A RADIAN
        DATA HASRAD/13750.98708D0/      ! HOUR ANGLE SECONDS IN A RADIAN
