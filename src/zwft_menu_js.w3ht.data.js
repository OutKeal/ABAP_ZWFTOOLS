function form_submit(trans) {
    var form = document.createElement('form');
    form.method = 'post';
    form.action = 'SAPEVENT:submit';
    var input = document.createElement('input');
    input.type = 'hidden';
    input.name = trans;
    input.value = trans;

    form.appendChild(input);
    document.body.appendChild(form);
    form.submit();
}
