open Hitscoreweb_std

let mime_types = "
application/atom+xml		atom
application/mathml+xml		mathml
application/msword		doc
application/ogg			ogg
application/pdf			pdf
application/postscript		ai eps ps
application/rdf+xml		rdf
application/smil		smi smil
application/srgs		gram
application/srgs+xml		grxml
application/vnd.mozilla.xul+xml	xul
application/vnd.ms-excel	xls
application/vnd.ms-powerpoint	ppt
application/x-dvi		dvi
application/x-javascript	js
application/x-koan		skp skd skt skm
application/x-latex		latex
application/x-netcdf		nc cdf
application/x-sh		sh
application/x-shar		shar
application/x-shockwave-flash	swf
application/x-tar		tar
application/x-tcl		tcl
application/x-tex		tex
application/x-texinfo		texinfo texi
application/x-troff		t tr roff
application/x-troff-man		man
application/x-troff-me		me
application/x-troff-ms		ms
application/xhtml+xml		xhtml xht
application/xslt+xml		xslt
application/xml			xml xsl
application/xml-dtd		dtd
application/xml-external-parsed-entity
application/zip			zip
audio/midi			mid midi kar
audio/mpeg			mpga mp2 mp3
audio/x-aiff			aif aiff aifc
audio/x-mpegurl			m3u
audio/x-pn-realaudio		ram ra
application/vnd.rn-realmedia	rm
audio/x-wav			wav
image/bmp			bmp
image/cgm			cgm
image/gif			gif
image/ief			ief
image/jpeg			jpeg jpg jpe
image/png			png
image/svg+xml			svg
image/tiff			tiff tif
image/x-cmu-raster		ras
image/x-icon			ico
image/x-portable-anymap		pnm
image/x-portable-bitmap		pbm
image/x-portable-graymap	pgm
image/x-portable-pixmap		ppm
image/x-rgb			rgb
image/x-xbitmap			xbm
image/x-xpixmap			xpm
image/x-xwindowdump		xwd
model/vrml			wrl vrml
text/calendar			ics ifb
text/css			css
text/html			html htm
text/plain			asc txt
text/richtext			rtx
text/rtf			rtf
text/sgml			sgml sgm
text/tab-separated-values	tsv
video/mpeg			mpeg mpg mpe
video/quicktime			qt mov
video/vnd.mpegurl		mxu m4u
video/x-msvideo			avi
"

let config ?(port=80) ~runtime_root ?conf_root kind =
  let conf_dir = Option.value ~default:runtime_root conf_root in
  let extensions =
    match kind with
    | `Ocsigen ->
      sprintf "
    <extension findlib-package=\"ocsigenserver.ext.staticmod\"/>
    <extension findlib-package=\"core\"/>
    <extension findlib-package=\"ocsigenserver.ext.ocsipersist-sqlite\">
      <database file=\"%s/ocsidb\"/>
    </extension>
    <extension findlib-package=\"eliom.server\"/> "
        runtime_root
    | `Static ->
      sprintf "
    <extension name=\"staticmod\"/>
    <extension name=\"ocsipersist\">
      <database file=\"%s/ocsidb\"/>
    </extension>
    <extension name=\"eliom\"/>" runtime_root
  in
  let hitscore_module =
    match kind with
    | `Ocsigen ->
      sprintf " <eliom module=\"_build/src/lib/hitscoreweb.cma\"/> "
    |`Static ->
      sprintf " <eliom name=\"hitscoreweb\"/> "
  in
  sprintf "
<ocsigen>
  <server>
    <port>%d</port>
    <user></user>
    <group></group>
    <commandpipe>%s/ocsigen_command</commandpipe>
    <mimefile>%s/mime.types</mimefile>

    <charset>utf-8</charset>
    <debugmode/>
%s
    <host hostfilter=\"*\">
      <static dir=\"%s/static/\" />
%s
    </host>
  </server>
</ocsigen>

"
port
runtime_root
conf_dir
extensions
runtime_root
hitscore_module

let syscmd s =
  match Unix.system s with
  | `Exited 0 -> Ok ()
  | e -> Error (Failure (Unix.Process_status.to_string_hum e))
let syscmd_exn s = syscmd s |> Result.raise_error


let testing kind =
  let runtime_root = "/tmp/hitscoreweb" in
  let port = 8080 in
  let exec =
    match kind with `Ocsigen -> "ocsigenserver" | `Static -> "hitscoreserver" in
  let open Result in
  syscmd (sprintf "mkdir -p %s" runtime_root) |> raise_error;
  Out_channel.(
    with_file (sprintf "%s/mime.types" runtime_root)
      ~f:(fun o -> output_string o (mime_types));
    with_file (sprintf "%s/hitscoreweb.conf" runtime_root)
      ~f:(fun o -> output_string o (config ~port ~runtime_root kind))
  );
  syscmd (sprintf "%s -c %s/hitscoreweb.conf" exec runtime_root) |> raise_error

let rpm_build () =
  let tmp_dir = "/tmp/hitscorerpmbuild" in
  let spec_file = sprintf "%s/SPECS/hitscoreweb.spec" tmp_dir in
  let binary_tmp = sprintf "%s/hitscoreserver" (Unix.getcwd ()) in
  let binary_dir = "/usr/libexec/hitscoreweb" in
  let mimes_tmp = (sprintf "%s/mime.types" tmp_dir) in
  let conf_tmp = (sprintf "%s/hitscoreweb.conf" tmp_dir) in
  let conf_root = "/etc/hitscoreweb" in
  let conf_target = (sprintf "%s/hitscoreweb.conf" conf_root) in
  let mimes_target = (sprintf "%s/mime.types" conf_root) in
  let runtime_root = "/var/run/hitscoreweb" in
  Out_channel.(
    with_file mimes_tmp
      ~f:(fun o -> output_string o (mime_types));
    with_file conf_tmp
      ~f:(fun o -> output_string o (config ~port:80 ~runtime_root ~conf_root `Static))
  );
  List.iter [ "BUILD"; "SPECS"; "RPMS"; "SOURCES"; "SRPMS" ]
    ~f:(fun dir ->
      sprintf "mkdir -p %s/%s" tmp_dir dir |> syscmd_exn);
  Out_channel.with_file spec_file
    ~f:(fun o ->
      fprintf o "%%define _topdir %s\n" tmp_dir;
      fprintf o "%%define name hitscoreweb\n";
      fprintf o "%%define release 1\n";
      fprintf o "%%define version 1.12\n";
      output_string o "
Name:   %{name}
Version: %{version}
Release:        1%{?dist}
Summary: Hitscoreweb web server and web app

Group:  Web Server
License: MIT
URL:    http://seb.mondet.org
#Source0:       
BuildRoot:      %(mktemp -ud %{_topdir}/%{name}-%{version}-%{release}-XXXXXX)\

Requires: libev openssl pcre sqlite zlib

%description
Hitscoreweb bundles an Ocsigen webserver with an embedded webapp.

%prep
# %setup -q
echo 'Preparing'

%build
echo 'Building'

%install
rm -rf $RPM_BUILD_ROOT
";
      fprintf o "mkdir -p $RPM_BUILD_ROOT/%s\n" binary_dir;
      fprintf o "mkdir -p $RPM_BUILD_ROOT/%s\n" conf_root;
      fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" binary_tmp binary_dir;
      fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" conf_tmp conf_root;
      fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" mimes_tmp conf_root;
      output_string o "
%clean
rm -rf $RPM_BUILD_ROOT

%files
";
      output_string o "%defattr(0555,root,root,-)\n";
      fprintf o "%s/hitscoreserver\n" binary_dir;
      output_string o "%defattr(0444,root,root,-)\n";
      fprintf o "%s\n" conf_target;
      fprintf o "%s\n" mimes_target;
      output_string o "

%doc

%changelog

";
    );
  syscmd_exn (sprintf "rpmbuild -v -bb --clean %s" spec_file)

let () =
  match Array.to_list Sys.argv with
  | [] | _ :: [] ->
    eprintf "Usage: hitscoreweb <command>\n"
  | exec :: "test" :: _ ->
    testing `Ocsigen
  | exec :: "static" :: _ ->
    testing `Static
  | exec :: "rpm" :: _ ->
    rpm_build ()
  | exec :: not_found :: _ ->
    eprintf "Unknown command: %s.\n" not_found
