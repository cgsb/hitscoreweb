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

let global_hitscore_configuration = ref None


let config ?(port=80) ~runtime_root ?conf_root ?log_root kind output_string =
  let conf_dir = Option.value ~default:runtime_root conf_root in
  let log_dir = Option.value ~default:runtime_root log_root in
  let extensions =
    match kind with
    | `Ocsigen ->
      sprintf "
    <extension findlib-package=\"ocsigenserver.ext.staticmod\"/>
    <extension findlib-package=\"core\"/>
    <extension findlib-package=\"pgocaml\"/>
    <extension findlib-package=\"hitscore\"/>
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
      sprintf " <eliom module=\"_build/src/lib/hitscoreweb.cma\">\n"
    |`Static ->
      sprintf " <eliom name=\"hitscoreweb\">\n"
  in
  sprintf "<ocsigen>\n  <server>\n    <port>%d</port>\n" port |> output_string;
  sprintf "    <user></user> <group></group>\n" |> output_string;
  sprintf "    <logdir>%s</logdir>\n" log_dir |> output_string;
  sprintf "    <commandpipe>%s/ocsigen_command</commandpipe>\n"
    runtime_root |> output_string;

  sprintf "    <mimefile>%s/mime.types</mimefile>\n" 
    conf_dir |> output_string;
  output_string " <charset>utf-8</charset> <debugmode/>\n";
  output_string extensions;
  ksprintf output_string "<host hostfilter=\"*\">\n <static dir=\"%s/\" /> %s\n"
    runtime_root hitscore_module;
  Hitscore_lwt.Configuration.(Option.(
    !global_hitscore_configuration
    >>= fun c ->
    db_host c >>= fun host ->
    db_port c >>= fun port ->
    db_username c >>= fun user ->
    db_password c >>= fun pswd ->
    db_database c >>= fun dbnm ->
    ksprintf output_string "  <pghost>%s</pghost>\n" host;
    ksprintf output_string "  <pgport>%d</pgport>\n" port;
    ksprintf output_string "  <pgdb>%s</pgdb>\n" dbnm;
    ksprintf output_string "  <pguser>%s</pguser>\n" user;
    ksprintf output_string "  <pgpass>%s</pgpass>\n" pswd;
    return None)) |! Pervasives.ignore;
  ksprintf output_string "</eliom>\n";
  ksprintf output_string "</host>\n";

  output_string "  </server>\n</ocsigen>\n";
  ()

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
  let open Out_channel in
  with_file (sprintf "%s/mime.types" runtime_root)
    ~f:(fun o -> output_string o (mime_types));
  with_file (sprintf "%s/hitscoreweb.conf" runtime_root)
    ~f:(fun o -> config ~port ~runtime_root kind (output_string o));
  syscmd (sprintf "%s -c %s/hitscoreweb.conf" exec runtime_root) |> raise_error


let sysv_init_file
    ~path_to_binary 
    ?(config_file="/etc/hitscoreweb/hitscoreweb.conf")
    ?(pid_file="/tmp/hitscoreweb/pidfile")
    ?(stdout_stderr_file="/tmp/hitscoreweb/stdout_stderr")
    ?(log_dir="/tmp/hitscoreweb/log/")
    ?(command_pipe="/tmp/hitscoreweb/ocsigen_command")
    output_string =
  let centos_friendly_header =
    sprintf  "#! /bin/sh
#
# hitscoreweb
#
# chkconfig: 345 99 01
# description:  Starts and stops Hitscoreweb
#
# config: %s
#
### BEGIN INIT INFO
# Provides: hitscoreweb
# Required-Start:  $syslog $network postgresql
# Required-Stop:  $syslog $network postgresql
# Default-Start: 3 4 5
# Default-Stop: 0 1 6
# Short-Description:  Hitscoreweb HTTP server
# Description: Start and stop Hitscoreweb
### END INIT INFO

# This file has been auto-generated, do not edit directly please.\
 
" config_file in
  let begining =
    sprintf "
set -e
case \"$1\" in" in
  let check_running ~do_then ?do_else () =
    sprintf "
    if [ -r \"%s\" ] && read pid < \"%s\" && ps -p \"$pid\" > /dev/null 2>&1; then
%s
%s
    fi
"   pid_file pid_file do_then 
      Option.(value ~default:"" (map ~f:(sprintf "else\n%s") do_else))
  in
  let start_case =
    sprintf "
   start|force-start)\
%s
    mkdir -p %s %s
    nohup %s --verbose --pidfile %s -c %s \
        > %s 2>&1 &
    ;;
"
      (check_running () ~do_then:"   echo \"Hitscoreweb is already running\"\n\
                              \   exit 0")
      (Filename.dirname stdout_stderr_file) log_dir
      path_to_binary pid_file config_file
      stdout_stderr_file
  in
  let stop_case = 
    sprintf "
  stop)
    echo -n \"Stopping Hitscoreweb: \"
    %s
    rm -f %s
    echo \"Done.\"
    ;;
" 
      (check_running ()
         ~do_then:(sprintf
                     "    echo shutdown > %s" command_pipe)
         ~do_else:"    echo \"Hitscoreweb was not running\"")
      pid_file in
  let kill_case = 
    sprintf "
  force-stop)
    echo -n \"Killing Hitscoreweb: \"
    for pid in `cat %s`; do kill -9 $pid || true; done
    rm -f %s
    echo \"Done.\"
    ;;
" 
      pid_file pid_file in
  let reload_case = 
    sprintf "
  reload)
    echo -n \"Reloading Hitscoreweb: \"
    echo reload > %s
    echo \"Done.\"
    ;;
" command_pipe in
  let status_case =
    sprintf "
  status)
    echo -n \"Status of Hitscoreweb: \"
    if [ ! -r \"%s\" ]; then
      echo \"It is NOT running.\"
      exit 3
    fi
    if read pid < \"%s\" && ps -p \"$pid\" > /dev/null 2>&1; then
      echo \"It IS running (pid: $pid).\"
      exit 0
    else
      echo \"it is NOT running BUT %s exists.\"
      exit 1
    fi
    ;;
" pid_file pid_file pid_file
  in
  let esac =
    sprintf "
  *)
    echo \"Usage: $0 {start|stop|reload|status|force-stop}\" >&2
    exit 1
    ;;
esac

exit 0
" in
  output_string centos_friendly_header;
  output_string begining;
  output_string start_case;
  output_string stop_case;
  output_string kill_case;
  output_string reload_case;
  output_string status_case;
  output_string esac


let rpm_build () =
  let () =
    match Unix.system 
      "test \"`ocamlfind list | grep batteries | grep 1.4 | wc -l`\" -eq 0"
    with
    | `Exited 1 -> 
      printf
        "ERR: It seems to be the wrong version of Batteries, \
          I won't continue:\n";
      printf "$ ocamlfind list | grep batteries\n%!";
      syscmd_exn "ocamlfind list | grep batteries";
      failwith "Batteries_version"
    | _ -> ()
  in
  let tmp_dir = "/tmp/hitscorerpmbuild" in
  let spec_file = sprintf "%s/SPECS/hitscoreweb.spec" tmp_dir in
  let binary_tmp = sprintf "%s/hitscoreserver" (Unix.getcwd ()) in
  let binary_dir = "/usr/libexec/hitscoreweb" in
  let binary_target = sprintf "%s/hitscoreserver" binary_dir in
  let conf_tmp = (sprintf "%s/hitscoreweb.conf" tmp_dir) in
  let conf_root = "/etc/hitscoreweb" in
  let conf_target = (sprintf "%s/hitscoreweb.conf" conf_root) in
  let mimes_tmp = (sprintf "%s/mime.types" tmp_dir) in
  let mimes_target = (sprintf "%s/mime.types" conf_root) in
  let sysv_tmp = sprintf "%s/hitscoreweb.init" tmp_dir in
  let sysv_target = "/etc/init.d/hitscoreweb" in
  let runtime_root = "/var/run/hitscoreweb" in

  let log_dir = "/var/log/hitscoreweb" in

  let open Out_channel in

  List.iter [ "BUILD"; "SPECS"; "RPMS"; "SOURCES"; "SRPMS" ]
    ~f:(fun dir ->
      sprintf "mkdir -p %s/%s" tmp_dir dir |> syscmd_exn);

  with_file mimes_tmp
    ~f:(fun o -> output_string o (mime_types));

  with_file conf_tmp ~f:(fun o -> 
    config ~port:80 ~runtime_root ~conf_root 
      ~log_root:log_dir `Static (output_string o));
  
  with_file sysv_tmp ~f:(fun o ->
    sysv_init_file
      ~path_to_binary:binary_target
      ~config_file:conf_target
      ~pid_file:(sprintf "%s/pidfile" runtime_root)
      ~stdout_stderr_file:(sprintf "%s/stdout_and_stderr" runtime_root)
      ~command_pipe:(sprintf "%s/ocsigen_command" runtime_root)
      ~log_dir
      (output_string o)
  );

  with_file spec_file ~f:(fun o ->
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
    fprintf o "mkdir -p $RPM_BUILD_ROOT/etc/init.d\n";
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" binary_tmp binary_dir;
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" conf_tmp conf_root;
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" mimes_tmp conf_root;
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" sysv_tmp sysv_target;

    output_string o "
%clean
rm -rf $RPM_BUILD_ROOT

%files
";
    output_string o "%defattr(0555,root,root,-)\n";
    fprintf o "%s/hitscoreserver\n" binary_dir;
    fprintf o "%s\n" sysv_target;
    output_string o "%defattr(0444,root,root,-)\n";
    fprintf o "%s\n" conf_target;
    fprintf o "%s\n" mimes_target;
    output_string o "

%doc

%changelog

%post
# Register the service
/sbin/chkconfig --add hitscoreweb

%preun
if [ $1 = 0 ]; then
        /sbin/service hitscoreweb stop > /dev/null 2>&1
        /sbin/chkconfig --del hitscoreweb
fi
";
  );
  syscmd_exn (sprintf "rpmbuild -v -bb --clean %s" spec_file)

let () =
  match Array.to_list Sys.argv with
  | [] | _ :: [] | _ :: _ :: [] ->
    eprintf "Usage: hitscoreweb <[config-file:]profile> <command>\n"
  | exec :: profile :: cmd_args ->
    let config_file, profile_name =
      match String.split profile ~on:':' with
      | [ one ] ->
        (sprintf "%s/.config/hitscore/config.sexp"
           (Option.value_exn_message "This environment has no $HOME !"
              (Sys.getenv "HOME")), one)
      | [ one; two ] ->
        (one, two)
      | _ -> failwithf "Can't understand: %s" profile ()
    in
    let config = In_channel.(with_file config_file ~f:input_all) in
    let hitscore_config =
      let open Result in
      Hitscore_lwt.Configuration.(
        parse_str config >>= fun c -> use_profile c profile_name)
      |! function
        | Ok o -> o
        | Error (`configuration_parsing_error e) ->
          eprintf "Error while parsing configuration: %s\n" (Exn.to_string e);
          failwith "STOP"
        | Error (`profile_not_found s) ->
          eprintf "Profile %S not found in config-file\n" s;
          failwith "STOP"
    in      
    global_hitscore_configuration := Some hitscore_config;
    begin match cmd_args with
    | [] -> printf "Nothing to do\n"
    | "test" :: _ -> testing `Ocsigen
    | "static" :: _ -> testing `Static
    | "rpm" :: _ -> rpm_build ()
    | "sysv" :: _ -> sysv_init_file ~path_to_binary:"/bin/hitscoreweb" print_string
    | not_found :: _ -> eprintf "Unknown command: %s.\n" not_found
    end
