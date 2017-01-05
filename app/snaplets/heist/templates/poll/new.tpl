<apply template="base">
    <bind tag="pagetitle">New poll</bind>
    <div class='row'>
        <div class='col-sm-4'>
            <form method="POST">
                <fieldset>
                    <div class="form-group">
                        <div class='row'>
                            <div class='col-sm-12'>

                                <label for="title">Title</label>
                                <input type="textemail" class="form-control" name="title" id="title" required="required" />
                                <br/>

                                <label for="description">Description</label><br />
                                <textarea id="description" name="description" class="form-control"></textarea>
                            </div>
                        </div>

                        <div class='row'>
                            <div class='col-sm-12'>
                                <label for="start">Start</label><br />
                                <div class='input-group date' id='startpicker'>
                                    <input type='text' class="form-control" required="required" name="start" id="start" />
                                    <span class="input-group-addon">
				      <span class="glyphicon glyphicon-calendar">
				      </span>
                                    </span>
                                </div>
                            </div>
                        </div>

                        <div class='row'>
                            <div class='col-sm-12'>
                                <label for="end">End</label><br />
                                <div class='input-group date' id='endpicker'>
                                    <input type='text' class="form-control" required="required" name="end" id="end" />
                                    <span class="input-group-addon">
				      <span class="glyphicon glyphicon-calendar">
				      </span>
                                    </span>
                                </div>
                            </div>
                        </div>

                        <div class='row'>
                          <div class='col-sm-12'>
			    <label for="end">Type</label><br />
                                <div class="dropdown">
                                    <button class="btn btn-default dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
				      Pick
				      <span class="caret">
				      </span>
				    </button>
                                    <ul class="dropdown-menu" aria-labelledby="dropdownMenu1">
                                        <li><a href="#">OR</a></li>
                                        <li><a href="#">List</a></li>
                                        <li role="separator" class="divider"></li>
                                        <li><a href="#">Custom</a></li>
                                    </ul>
                                </div>
                            </div>
                        </div>

                        <div class='row'>
                            <div class="col-sm-12">
                                <label for="">Image</label><br />
                                <div class="input-group">
                                    <span class="input-group-btn">
				  <span class="btn btn-primary btn-file">
				    Browse… <input type="file" multiple="">
				  </span>
                                    </span>
                                    <input type="text" class="form-control" readonly="">
                                </div>
                            </div>
                        </div>
                        <div class='row'>
                            <div class="col-sm-12" style="float:left">
                                <label for="submit"></label><br />
                                <input type="submit" value="Publish" class="btn btn-primary" />
				<input type="submit" value="Preview" class="btn btn-default" />
                            </div>
                        </div>
                    </div>
                </fieldset>
            </form>
        </div>
        <div class='col-sm-8'>
            <a href="#" class="img-thumbnail">
                <img src="/static/img/prez2.png" alt="...">
            </a>
        </div>
    </div>
</apply>
