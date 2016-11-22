(require racket/system)

;;Otwieranie plików
(define (otworz-plik plik)
  (define sciezka plik)
  (let ((p (open-input-file plik)))
    (let f ((x (read p)))
      (if (eof-object? x)
          (begin
            (close-input-port p)
            '())
          (cons x (f (read p)))))))

(define (czytaj sciezka)
  (display "Zawartość pliku ")(display sciezka)(display ":\n")
  (write (otworz-plik sciezka)))

;;Usuwanie plików
(define (usun plik)
  (delete-file plik))

;;Zmienianie nazwy plików
(define (zmien-nazwe plik nowa-nazwa)
  (rename-file-or-directory plik nowa-nazwa))

;;Napisz hello
(define (napisz-hello)
  (display "Hello!"))

;;Pokaż status programu
(define (prog-status)
  (display "Spier*alaj"))

;;Wyłącz komputer
(define (wyl-komp)
  (display "!!! Wyłączanie komputera. Wpisz hasło, aby kontynuować...")
  (system "xterm -e sudo shutdown 0"))

;;Restartuj komputer
(define (reboot)
  (display "!!! Restartowanie komputera. Wpisz hasło, aby kontynuować...")
  (system "xterm -e sudo reboot"))

;;Wybierz losowy plik z folderu
(define (choose-random-file folder)
  (let* ((lista-plikow (directory-list folder)) (dlg (length lista-plikow))
    (plik (list-ref lista-plikow (random dlg)))) plik))

;;Utwórz nowy plik
(define (utworz-plik nazwa)
  (let ((p (open-output-file nazwa 'append)))
    (close-output-port p))
  (system (string-append "xterm -e nano " nazwa)))

;;Otwórz firefoxa
(define (firefox)
  (system "firefox"))


;;Otwórz e-mail
(define (mail)
  (display "Otwieranie skrzynki mailowej...")
  (system (string-append "firefox http://www.gmail.com")))

(mail)
