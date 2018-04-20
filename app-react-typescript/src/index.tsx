import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { Store } from 'redux';
import { Router, browserHistory } from 'react-router';
import * as injectTapEventPlugin from 'react-tap-event-plugin';
import createRoutes from './routes';
import { PollsModule } from './modules';
import configureStore from './configureStore';
import { IModule, IReduxState } from './shared/types/app';
import Api from './shared/api/Api';

// Needed for onTouchTap: http://stackoverflow.com/a/34015469/988941
injectTapEventPlugin();

/* Prepare main app elements */
const history = browserHistory;
const modules: Array<IModule<any>> = [ new PollsModule() ];
const api = new Api('/api');
const store = configureStore(modules, api);
const routes = createRoutes(modules);
const rootComponent = (
  <Provider store={store}>
    <Router ref={onRouterRef} history={history} routes={routes} />
  </Provider>
);

function onRouterRef(ref: any) {
  modules.forEach(module => {
    module.router = ref.router;
    module.store = store as Store<IReduxState>;
  });
}

/* Start application */
if (process.env.NODE_ENV !== 'test') {
  ReactDOM.render(rootComponent, document.getElementById('root'));
}
