# Приложение для краулинга страниц с сайта ScienceDaily
Данный краулер:
- собирает 100 последних [статей о квантовых компьютерах](https://www.sciencedaily.com/news/computers_math/quantum_computers)
- Парсит HTML убирая картинки, ссылки, скрипты и стили
- Сохраняет страницы в папку ```./Downloads``` и создает ```index.txt```, в котором хранятся пары (```имя.файла``` ссылка) 