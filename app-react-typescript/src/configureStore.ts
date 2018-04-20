import {
  compose,
  applyMiddleware,
  combineReducers,
  createStore,
  Reducer,
  Middleware,
  Store,
  ReducersMapObject,
} from 'redux';
import thunk from 'redux-thunk';
import { reducer as createPollReducer } from './features/editPoll';
import { reducer as viewPollsReducer } from './features/viewPolls';
import { IModule, IReduxState, IExtraArguments } from './shared/types/app';
import Api from './shared/api/Api';

function configureStore(modules: Array<IModule<any>>, api: Api): Store<Object> {
  const extraArguments: IExtraArguments = { api };
  const middlewares: Middleware[] = [
    thunk.withExtraArgument(extraArguments),
  ];

  const modulesReducers: ReducersMapObject = modules.reduce((reducers, module) => {
    if (module.getReducer) {
      const reducerData = module.getReducer();
      reducers[reducerData.name] = reducerData.reducer;
    }

    return reducers;
  }, {} as ReducersMapObject);

  const reducer: Reducer<IReduxState> = combineReducers<IReduxState>({
    createPoll: createPollReducer,
    viewPolls: viewPollsReducer,
    ...modulesReducers,
  });

  return createStore(
    reducer,
    compose(
      applyMiddleware(...middlewares),
      ('development' === process.env.NODE_ENV && window.devToolsExtension)
        ? window.devToolsExtension() : ((arg: any) => arg),
    ),
  );
}

export default configureStore;
