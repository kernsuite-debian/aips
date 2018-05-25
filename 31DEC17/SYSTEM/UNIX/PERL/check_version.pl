# -*-Perl-*-
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1997, 2000-2001, 2004
#;  Associated Universities, Inc. Washington DC, USA.
#;
#;  This program is free software; you can redistribute it and/or
#;  modify it under the terms of the GNU General Public License as
#;  published by the Free Software Foundation; either version 2 of
#;  the License, or (at your option) any later version.
#;
#;  This program is distributed in the hope that it will be useful,
#;  but WITHOUT ANY WARRANTY; without even the implied warranty of
#;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#;  GNU General Public License for more details.
#;
#;  You should have received a copy of the GNU General Public
#;  License along with this program; if not, write to the Free
#;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
#;  MA 02139, USA.
#;
#;  Correspondence concerning AIPS should be addressed as follows:
#;         Internet email: aipsmail@nrao.edu.
#;         Postal address: AIPS Project Office
#;                         National Radio Astronomy Observatory
#;                         520 Edgemont Road
#;                         Charlottesville, VA 22903-2475 USA
#-----------------------------------------------------------------------

package Check_Perl_Version;

@tested_versions = (
                    "4.010",
                    "4.036",
                    "5.000",
                    "5.001",
                    "5.002",
                    "5.003",
		    "5.004",
		    "5.005",
		    "5.006",
		    "5.007",
		    "5.008",
                    );

sub main'check_perl_version #'
{
    local ($calling_program) = $0;
    $calling_program =~ s/.*\///;
    local ($perl_version) = sprintf ("%5.3f", $]);
    grep (/$perl_version/o, @tested_versions) ||
        warn "$calling_program: This program is untested under Perl version $perl_version\n";
}

1;
