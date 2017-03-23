module CsrfCookie exposing (csrfCookie)

import Native.CsrfCookie
import Task


csrfCookie : () -> Task.Task () String
csrfCookie =
    Native.CsrfCookie.csrfCookie
