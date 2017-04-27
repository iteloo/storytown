# Storytown

Storytown is an online teaching portal for language teachers and students

Teachers can:
- Quickly set up lessons for their students
- Enter translations for the readings
- Record passages directly in browsers
- Track student progress on the dashboard

Students can:
- Review passages with smart reader
- Listen to passages with audio

## Technical note

Storytown is built using Javascript/Elm on the frontend and a RESTful Haskell [servant](https://github.com/haskell-servant/servant) API on the backend! The recorded audio resources are stored on Amazon S3, and lesson/user data are persisted in Postgres through [Persistent](https://github.com/yesodweb/persistent). 
